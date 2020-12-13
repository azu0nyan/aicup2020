package strategy

import model.EntityType._
import model._

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object ProductionLogic extends StrategyPart {

  case class Composition(workers: Int, melee: Int, ranged: Int) {
    def foodReq: Int = workers + melee + ranged
  }


  val lastWorkerProductionTick = 850
  val maxWorkersToResourcesRatio = 6
  val maxWorkersWhenEnclosed = 50
  val meleeToArchersRatio = 0.15

  val compositionFrames: Seq[Composition] = Seq(
    Composition(1, 1, 1),
    Composition(11, 1, 1),
    Composition(12, 1, 1),
    Composition(13, 1, 1),
    Composition(13, 2, 1),
    Composition(13, 2, 2),
    Composition(15, 2, 2),
    Composition(15, 2, 2),
    Composition(15, 2, 3),
    Composition(20, 2, 5),
    Composition(22, 4, 10),
    Composition(25, 4, 15),
    Composition(30, 6, 20),
    Composition(35, 6, 24),
    Composition(40, 6, 26),
    Composition(45, 6, 28),
    Composition(46, 6, 32),
    Composition(50, 6, 36),
    Composition(55, 6, 40),
    Composition(55, 6, 60),
    Composition(55, 6, 80),
    Composition(55, 6, 90),
    Composition(75, 6, 150),
    Composition(75, 6, 200),
  )





  def productionQueueForRecommendedComposition(implicit gameInfo: GameInfo): Seq[EntityType] = {
    if (gameInfo.populationFree > 0) {
      val workers = gameInfo.my(BUILDER_UNIT).size
      val melee = gameInfo.my(MELEE_UNIT).size
      val ranged = gameInfo.my(RANGED_UNIT).size
      val canProduceWorker = gameInfo.my(BUILDER_BASE).exists(_.active)
      val canProduceMelee = gameInfo.my(MELEE_BASE).exists(_.active)
      val canProduceRanged = gameInfo.my(RANGED_BASE).exists(_.active)

      val (nw, nm, nr) = compositionFrames.find { case Composition(w, m, r) =>
        (canProduceWorker && workers < w) | (canProduceMelee && melee < m) | (canProduceRanged && ranged < r)
      }.map {
        case Composition(w, m, r) =>
          (
            if (canProduceWorker && workers < w) w - workers else 0,
            if (canProduceMelee && melee < m) m - melee else 0,
            if (canProduceRanged && ranged < r) r - ranged else 0,
          )
      }.getOrElse((0, 0, 0))


      ((0 until nw).take(gameInfo.my(BUILDER_BASE).size).map(_ => BUILDER_UNIT) ++
        (0 until nm).take(gameInfo.my(MELEE_BASE).size).map(_ => MELEE_UNIT) ++
        (0 until nr).take(gameInfo.my(RANGED_BASE).size).map(_ => RANGED_UNIT)).take(gameInfo.populationFree)
    } else Seq()
  }

  def productionQueueMatchStrongest(implicit gameInfo: GameInfo): Seq[EntityType] = {
    val powerToMax = gameInfo.macroState.playerPowers.values.max - gameInfo.macroState.myPower
    if (powerToMax >= 15) Seq(RANGED_UNIT)
    else if (powerToMax >= 10) Seq( RANGED_UNIT)
    else if (powerToMax >= 5) Seq(RANGED_UNIT)
    else Seq()
    Seq()
  }


  //  val minWorkers: Int = 5
  //
  //  val workersToArmyRatio: Double = 0.5
  //
  //  val rangedToArmyRatio: Double = 0.4


  //  def targetRanged(implicit g: GameInfo):Int = g.a

  def canProduce(u: EntityType)(implicit g: GameInfo): Boolean =
    g.populationFree >= u.populationUse && g.myMinerals >= g.unitCost(u) &&
      g.my(unitToBuilderBase(u)).exists(b => b.active && !g.reservedBuildings.contains(b))


  def possibleSpawnPlaces(around: Entity)(implicit g: GameInfo): Seq[(Int, Int)] =
    rectNeighbours(around.position.x, around.position.y, around.entityType.size, g.mapSize, g.mapSize)
      .filter { case (x, y) => g.emptyNextTurn(x, y) }

  //  def findFreeCellAround(e: Entity)(implicit g: GameInfo): Option[(Int, Int)] =
  //    rectNeighbours(e.position.x, e.position.y, e.entityType.size, g.mapSize, g.mapSize)
  //    .find(g.cellToEntity(_).isEmpty)

  def maxResourceDistanceMatter = 30
  def findCellToSpawnBuilders(spawner: Entity)(implicit g: GameInfo): Option[(Int, Int)] = {
    possibleSpawnPlaces(spawner) match {
      case Seq() => None
      case cells =>
        g.resources.filter(r => r.position.distanceTo(spawner.position) < maxResourceDistanceMatter && !g.minableResource.contains(r)) match {
          case Seq() => Some(cells.maxBy { case (x, y) => x + y })
          case resources =>
            Some(cells.map(c => (c, resources.map(r => distance(c, r.position.toProd)).min)).minBy(_._2)._1)
        }
    }
  }

  def findCellToSpawnMelee(spawner: Entity)(implicit g: GameInfo): Option[(Int, Int)] = {
    possibleSpawnPlaces(spawner) match {
      case Seq() => None
      case cells =>
        g.enemyEntities.filter(_.entityType == RANGED_UNIT)
          .minByOption(e => distanceFromSquare(spawner.position.toProd, spawner.entityType.size, e.position.toProd))
          .orElse(g.enemyEntities.filter(_.entityType == MELEE_UNIT)
            .minByOption(e => distanceFromSquare(spawner.position.toProd, spawner.entityType.size, e.position.toProd))) match {
          case Some(enemy) =>
            Some(cells.minBy(c => distance(c, enemy.position.toProd)))
          case None => Some(cells.maxBy { case (x, y) => x + y })
        }
    }
  }

  def findCellToSpawnRanged(spawner: Entity)(implicit g: GameInfo): Option[(Int, Int)] = {
    possibleSpawnPlaces(spawner) match {
      case Seq() => None
      case cells =>
        val reg = g.region(spawner.position)
        val inDanger = reg.danger9 >= 5
        if (inDanger) {
          val meleeClose = reg.enemyN9(Seq(MELEE_UNIT))
          val rangedClose = reg.enemyN9(Seq(MELEE_UNIT))

          //          val noDangerCells = cells.filter(c => g.dangerMap(c.x)(c.y) < 5)
          //spawn in range but 2 cells from melee

          val closestMelee: (Int, Int) => Option[Int] = (x, y) => meleeClose.map(m => distance((x, y), m.position.toProd)).minOption

          val duelCell: Seq[(Int, Int)] => Option[(Int, Int)] = s => if(meleeClose.isEmpty) None else {s.filter(c => g.dangerMap(c.x)(c.y) == 5 &&
            rectNeighbours(c.x, c.y, 2, g.mapSize, g.mapSize).forall { case (x, y) => g.dangerMap(x)(y) <= 5 } &&
            (closestMelee(c.x, c.y).isEmpty || closestMelee(c.x, c.y).get > 2))
            .maxByOption(c => meleeClose.map(m => distance(c, m.position.toProd)).min)
          }

          val safeFightMeleeCell: Seq[(Int, Int)] => Option[(Int, Int)] = s => s.filter(c => g.dangerMap(c.x)(c.y) < 5 && {
            val cl = closestMelee(c.x, c.y)
            cl.nonEmpty && cl.get >= 3 && cl.get <= 5
          }).maxByOption(c => closestMelee(c.x, c.y))

          val safestCell: Seq[(Int, Int)] => Option[(Int, Int)] = s => s.groupBy(c => g.dangerMap(c.x)(c.y)).toSeq.minBy(_._1)._2
            .minByOption { case (x, y) => rectNeighbours(x, y, 1, g.mapSize, g.mapSize).map { case (xx, yy) => g.dangerMap(xx)(yy) }.sum }

          safeFightMeleeCell(cells).orElse(
            duelCell(cells)).orElse(
            safestCell(cells))
            .orElse(cells.minByOption { case (x, y) => x + y })


        } else Some(cells.maxBy { case (x, y) => x + y })
    }
  }

  def produce(unitType: EntityType)(implicit g: GameInfo): Option[(Int, EntityAction)] =
    g.my(unitToBuilderBase(unitType)).find(b => b.active && !g.reservedBuildings.contains(b)).flatMap { building =>
      val cell:Option[(Int, Int)] = unitType match {
        case BUILDER_UNIT => findCellToSpawnBuilders(building)
        case MELEE_UNIT => findCellToSpawnMelee(building)
        case RANGED_UNIT =>findCellToSpawnRanged(building)
        case _ => None
      }
      cell.map(x => g.spawn(building, unitType, x))




    }


  override def getActions(implicit g: GameInfo): ActionMap = {

    val res: mutable.Map[Int, EntityAction] = mutable.Map[Int, EntityAction]()


    var prodQueue:Seq[EntityType] = Seq()//productionQueueMatchStrongest ++ productionQueueForRecommendedComposition

    if(g.macroState.noPathToEnemy && g.myWorkers.size < maxWorkersWhenEnclosed && canProduce(BUILDER_UNIT)){
      prodQueue = BUILDER_UNIT +: prodQueue
    }

    if(g.macroState.ourPowerAtBase * 1.2 < g.macroState.enemyPowerAtOurBase ) {
      if (g.myMeleeUnits.size * meleeToArchersRatio < g.myRangedUnits.size * (1f - meleeToArchersRatio)) {
        prodQueue = MELEE_UNIT +: prodQueue
      } else {
        prodQueue = RANGED_UNIT +: prodQueue
      }
    } else prodQueue =prodQueue ++  productionQueueForRecommendedComposition

    if(g.resources.size < g.myWorkers.size * maxWorkersToResourcesRatio || g.pw.currentTick > lastWorkerProductionTick)  prodQueue = prodQueue.filter(_ != BUILDER_UNIT)
      //    }
//    println(s"${g.populationUse} / ${g.populationMax} ${prodQueue}")
    prodQueue.foreach { e =>
      if (canProduce(e)) produce(e).foreach(res += _)
    }

    g.myBuildings.filter(b => !g.reservedBuildings.contains(b)).foreach {
      b => res += b.id -> EntityAction(None, None, None, None)
    }

    res.toMap
  }
}

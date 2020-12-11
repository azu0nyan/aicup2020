package strategy

import model.EntityType._
import model._

import scala.collection.mutable

object ProductionLogic extends StrategyPart {

  case class Composition(workers: Int, melee: Int, ranged: Int) {
    def foodReq: Int = workers + melee + ranged
  }

  val compositionFrames: Seq[Composition] = Seq(
    Composition(1, 1, 1),
    Composition(11, 1, 1),
    Composition(12, 1, 1),
    Composition(13, 1, 1),
    Composition(13, 2, 1),
    Composition(13, 3, 2),
    Composition(15, 3, 2),
    Composition(15, 3, 2),
    Composition(15, 4, 3),
    Composition(20, 5, 5),
    Composition(20, 5, 10),
    Composition(25, 10, 20),
    Composition(30, 10, 30),
    Composition(40, 15, 40),
    Composition(45, 20, 60),
    Composition(50, 25, 70),
    Composition(50, 25, 80),
    Composition(50, 25, 90),
    Composition(70, 35, 150),
    Composition(75, 40, 200),
  )

  def isHouseRequired(currentPop: Int, popMax: Int): Boolean = {
    if (popMax < 15) true
    else if (popMax == 15) currentPop >= 14
    else if (popMax == 20) currentPop >= 18
    else if (popMax == 25) currentPop >= 23
    else if (popMax == 30) currentPop >= 26
    else if (popMax == 35) currentPop >= 29
    else popMax <= currentPop + 10

  }


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
    val powerToMax = gameInfo.playerPowers.values.max -  gameInfo.myPower
    if(powerToMax >= 15) Seq(RANGED_UNIT, MELEE_UNIT)
    else if(powerToMax >= 10) Seq(MELEE_UNIT, RANGED_UNIT)
    else if(powerToMax >= 5) Seq(MELEE_UNIT)
    else Seq()
  }



  //  val minWorkers: Int = 5
  //
  //  val workersToArmyRatio: Double = 0.5
  //
  //  val rangedToArmyRatio: Double = 0.4


  //  def targetRanged(implicit g: GameInfo):Int = g.a

  def canProduce(u: EntityType)(implicit g: GameInfo): Boolean =
    g.populationFree >= u.populationUse && g.myMinerals >= g.unitCost(u) && g.my(unitToBuilderBase(u)).exists(b => b.active && !g.reservedBuildings.contains(b))


  def findFreeCellAround(e: Entity)(implicit g: GameInfo): Option[(Int, Int)] = rectNeighbours(e.position.x, e.position.y, e.entityType.size, g.mapSize, g.mapSize)
    .find(g.cellToEntity(_).isEmpty)


  def produce(unitType: EntityType)(implicit g: GameInfo): Option[(Int, EntityAction)] =
    g.my(unitToBuilderBase(unitType)).find(b => b.active && !g.reservedBuildings.contains(b)).flatMap { building =>
      findFreeCellAround(building).map { producePosition =>
        println(s"Producing $unitType at $producePosition")
        g.populationFree -= unitType.populationUse //Update resource status
        g.myMinerals -= g.unitCost(unitType)
        g.reservedBuildings += building
        (building.id,
          EntityAction(
            None,
            Some(BuildAction(unitType, producePosition)),
            None,
            None
          )
        )
      }
    }


  override def getActions(implicit g: GameInfo): ActionMap = {

    val res: mutable.Map[Int, EntityAction] = mutable.Map[Int, EntityAction]()


    val housesReq = isHouseRequired(g.populationUse, g.populationMaxWithNonactive)
    if (housesReq && g.myMinerals >= HOUSE.initialCost) {
      println(s"Trying to build house")
      BuildingLogic.build(HOUSE) match {
        case Some(command) =>
          g.myMinerals -= HOUSE.buildScore
          res += (command._1.id -> command._2)
        case None =>
      }
    }

    val prodQueue = productionQueueMatchStrongest ++ productionQueueForRecommendedComposition
    println(s"${g.populationUse} / ${g.populationMax} ${prodQueue}")
    prodQueue.foreach { e =>
      if (canProduce(e)) produce(e).foreach(res += _)
    }

    g.myBuildings.filter(b => !g.reservedBuildings.contains(b)).foreach {
      b => res += b.id -> EntityAction(None, None, None, None)
    }

    res.toMap
  }
}

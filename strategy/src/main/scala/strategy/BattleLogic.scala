package strategy

import model._
import model.EntityType._

import org.graalvm.compiler.lir.alloc.lsra.OptimizingLinearScanWalker

import scala.collection.mutable
import scala.util.Random


object BattleLogic extends StrategyPart {


  class RegionInfo(
                    val id: (Int, Int),
                    val min: (Int, Int)
                  )(implicit g: GameInfo) {


    def center: Vec2Int = Vec2Int(min.x, min.y) + Vec2Int(g.regionsSize / 2, g.regionsSize / 2)

    val my_ : mutable.Map[EntityType, Seq[Entity]] = mutable.Map()
    val enemy_ : mutable.Map[EntityType, Seq[Entity]] = mutable.Map()
    var resources: Int = 0
    def my(e: EntityType): Seq[Entity] = my_.getOrElse(e, Seq())
    def enemy(e: EntityType): Seq[Entity] = enemy_.getOrElse(e, Seq())

    def neighbours4: Seq[RegionInfo] = rectNeighbours(id._1, id._2, g.regionInSide, g.regionInSide).map { case (x, y) => g.regions(x)(y) }

    def neighbours9: Seq[RegionInfo] = neighbours9Pos(id._1, id._2, g.regionInSide, g.regionInSide).map { case (x, y) => g.regions(x)(y) }


    def freeCells: Int = g.regionsSize * g.regionsSize - resources / RESOURCE.maxHealth


    lazy val power: Int =
      this.my(RANGED_UNIT).size * 5 * 10 +
        this.my(MELEE_UNIT).size * 5 * 15 +
        this.my(TURRET).size * 5 * 30 +
        this.my(BUILDER_UNIT).size * 1

    lazy val danger: Int =
      this.enemy(RANGED_UNIT).size * 5 * 10 +
        this.enemy(MELEE_UNIT).size * 5 * 15 +
        this.enemy(TURRET).size * 5 * 30 +
        this.enemy(BUILDER_UNIT).size * 1
    lazy val defenceValue: Int =
      this.my(BUILDER_BASE).size * 1000 +
        this.my(MELEE_BASE).size * 1000 +
        this.my(RANGED_BASE).size * 1000 +
        this.my(BUILDER_UNIT).size * 100 +
        this.my(TURRET).size * 200
    lazy val reward: Int =
      enemy_.map { case (entityType, seq) => entityType.destroyScore * seq.length }.sum


    lazy val reward9: Int = neighbours9.map(_.reward).sum
    lazy val defence9: Int = neighbours9.map(_.defenceValue).sum
    lazy val danger9: Int = neighbours9.map(_.danger).sum
    lazy val power9: Int = neighbours9.map(_.power).sum

    lazy val entities: Seq[Entity] = my_.values.flatten ++ enemy_.values.flatten toSeq

    lazy val controlledByPlayerId: Option[Int] = entities.groupBy(_.playerId)
      .map { case (p, value) => (p, value.map(_.entityType.buildScore).sum) }.maxByOption(_._2).flatMap(_._1)
  }


  var importantRegions: Set[RegionInfo] = Set()
  def importantRegionToDefend(implicit g: GameInfo) = 5

  val defendRegionPrice = 10
  val defendWinRegionPrice = 8
  val attackRegionPrice = 8

  def importanceMap(implicit g: GameInfo): Seq[(RegionInfo, Int)] = {
    val (needDefendWeLose, needDefendWeWin) =
      g.allRegions.filter(x => x.defence9 > 0 && x.danger9 > 0).partition(x => x.power9 < x.danger9)
    val attackTargets =
      if (g.pw.currentTick < 100) Seq()
      else if (g.pw.currentTick < 250)
        g.allRegions.filter(x => x.reward9 > 0 && x.danger9 * 7 < g.myPower).sortBy(x => Vec2Int(0, 0).distanceTo(x.id)).take(3)
      else g.allRegions.filter(x => x.reward9 > 0 && x.danger9 * 7 < g.myPower).sortBy(-_.reward9).take(3)



    needDefendWeLose.map(x => (x, defendRegionPrice)) ++
      needDefendWeWin.map(x => (x, defendWinRegionPrice)) ++
      attackTargets.map(x => (x, attackRegionPrice))

  }

  def pf(implicit g: GameInfo): Array[Array[Int]] = {
    val res = Array.tabulate[Int](g.regionInSide, g.regionInSide)((x, y) => Int.MinValue)
    val queue: mutable.PriorityQueue[(Int, Int, Int)] = mutable.PriorityQueue()(Ordering.by(_._3))

    importanceMap.foreach {
      case (info, i) =>
        queue.addOne(info.id.x, info.id.y, i)
    }

    while (queue.nonEmpty) {
      val (x, y, cost) = queue.dequeue()
      if (res(x)(y) < cost) {
        res(x)(y) = cost
        //      neighbours9Pos(x, y, g.regionInSide, g.regionInSide)
        rectNeighbours(x, y, 1, g.regionInSide, g.regionInSide)
          .filter { case (x, y) => res(x)(y) == Int.MinValue }.foreach {
          case (x, y) =>
            val reg = g.regions(x)(y)

            queue.addOne(x, y, cost - (if (reg.freeCells > 15) 1 else 4))
        }
      }
    }
    res
  }

  override def getActions(implicit g: GameInfo): ActionMap = {


    importantRegions = importanceMap.map(_._1).toSet
    //defence
    //    importantRegions = g.allRegions.map(r => (r, r.neighbours9.map(_.danger).sum, r.neighbours9.map(_.power).sum, r.neighbours9.map(_.defenceValue).sum))
    //      .filter(_._4 > 0).filter(_._2 > 0).filter(x => x._2 >= x._3).sortBy(-_._4).map(_._1) toSet
    //
    //    if (g.myPower >= g.playerPowers.values.max) {
    //      importantRegions ++= g.allRegions.filter(_.reward > 0).sortBy(-_.reward).take(5)
    //    }

    val p = pf(g)


    (g.my(RANGED_UNIT) ++ g.my(MELEE_UNIT)).map { u =>
      val myReg = g.region(u.position)
      if (g.dangerMap(u.position.x)(u.position.y) > 0) {
        //battle ai
        (u.id, g.enemyEntities.minBy(e => u.position.distanceTo(e.position)) match {
          case e if u.position.distanceTo(e.position) <= u.entityType.attack.get.attackRange =>
            EntityAction(None, None, Some(AttackAction(Some(e.id), None)), None)
          case e =>
            EntityAction(None, None, Some(AttackAction(None, Some(AutoAttack(10, Seq())))), None)
          //                EntityAction(Some(MoveAction(e.position, true, true)), None, Some(AttackAction(Some(e.id), None)), None)
        })
      } else {
        g.enemyEntities.minBy(e => u.position.distanceTo(e.position)) match {
          case e if u.position.distanceTo(e.position) <= u.entityType.attack.get.attackRange =>
            (u.id, EntityAction(None, None, Some(AttackAction(Some(e.id), None)), None))
          case e if u.position.distanceTo(e.position) <= 5 =>
            (u.id, EntityAction(None, None, Some(AttackAction(None, Some(AutoAttack(3, Seq())))), None))
          //                EntityAction(Some(MoveAction(e.position, true, true)), None, Some(AttackAction(Some(e.id), None)), None)
          case _ =>
            //macroAi
            //defenceMode
            val myPotential = p(myReg.id.x)(myReg.id.y)
            val (target, targetPotential) = myReg.neighbours9.map(x => (x, p(x.id.x)(x.id.y))).maxBy(_._2)
            if (myPotential < targetPotential) {
              (u.id, EntityAction(Some(MoveAction(target.center, true, true)), None, None, None))
            } else {
              (u.id, EntityAction(Some(MoveAction(myReg.center, true, true)), None, None, None))
            }
        }
      }

    }.toMap

  }

  /*(g.my(RANGED_UNIT) ++ g.my(MELEE_UNIT)).map { u =>
    (u.id, g.enemyEntities.minBy(e => u.position.distanceTo(e.position)) match {
      case e if u.position.distanceTo(e.position) <= u.entityType.attack.get.attackRange =>
        EntityAction(None, None, Some(AttackAction(Some(e.id), None)), None)
      case e =>
        EntityAction(Some(MoveAction(e.position, true, true)), None, Some(AttackAction(Some(e.id), None)), None)

    })*/

}

//    g.my(RANGED_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap ++
//    g.my(MELEE_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap


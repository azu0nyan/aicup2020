package strategy

import model._
import model.EntityType._
import org.graalvm.compiler.lir.alloc.lsra.OptimizingLinearScanWalker

import scala.collection.mutable
import scala.util.Random

object BattleLogic extends StrategyPart {


  sealed trait RecommendedAction
  case object Flee extends RecommendedAction
  case object Stay extends RecommendedAction

  class RegionInfo(
                    var pos: (Int, Int),
                    var size: Int,
                    var enemyUnits: mutable.Map[EntityType, Seq[Entity]],
                    var myUnits: mutable.Map[EntityType, Seq[Entity]],
                    var resources: Int,
                    var recommendedAction: Option[RecommendedAction] = None
                  ) {
    def center: Vec2Int = toVec2Int(pos) + (size / 2, size/ 2)

    def reward: Int = enemyUnits.values.flatten.map(_.entityType.destroyScore).sum
    def enemyPower: Int = enemyUnits.values.flatten.flatMap(_.entityType.attack.map(_.damage)).sum
    def power: Int = myUnits.values.flatten.flatMap(_.entityType.attack.map(_.damage)).sum

  }

  val baseArea:Int = 30
  val regionSize: Int = 5

  val minValuableReward: Int = 20
  val holdPositionMultiplier: Double = 1.3
  val attackPositionMultiplier: Double = 0.7


  override def getActions(implicit g: GameInfo): ActionMap = {
    val regions = g.pw.mapSize / regionSize
    val regionInfos: Array[Array[RegionInfo]] = Array.tabulate(regions, regions)((x, y) =>
      new RegionInfo((x * regionSize, y * regionSize), regionSize, mutable.Map(), mutable.Map(), 0))

    def region(x:Int, y:Int) :RegionInfo = regionInfos(x / regionSize)(y / regionSize)

    g.pw.entities.foreach { e =>
      val cx = e.position.x / regionSize
      val cy = e.position.y / regionSize
      if (e.playerId.contains(g.me.id)) {
        regionInfos(cx)(cy).myUnits.updateWith(e.entityType) {
          case None => Some(Seq(e))
          case Some(s) => Some(s :+ e)
        }
      } else if (e.playerId.isDefined) {
        regionInfos(cx)(cy).enemyUnits.updateWith(e.entityType) {
          case None => Some(Seq(e))
          case Some(s) => Some(s :+ e)
        }
      } else {
        regionInfos(cx)(cy).resources += 1
      }

    }


    for (i <- 0 until regions; j <- 0 until regions) {
      val current = regionInfos(i)(j)
      val neighbourCords = rectNeighbours(i, j, 1, regions, regions)
      val neighbours = neighbourCords.map{ case (i, i1) => regionInfos(i)(i1)}

      def stayHereAndAttack:EntityAction = {
        val targets = if(current.enemyUnits.contains(RANGED_UNIT)) Seq(RANGED_UNIT, MELEE_UNIT) else Seq()
        EntityAction(None, None, Some(AttackAction(None, Some(AutoAttack(regionSize,targets)))), None)
      }

      def goToNeighbourToHelp:Option[EntityAction] = neighbours.filter(_.reward >= minValuableReward)
        .filter(x => x.enemyPower < (current.power + x.power) * attackPositionMultiplier).maxByOption(_.reward)
        .map(ri => EntityAction(Some(MoveAction(ri.center, true, false)), None, None, None))

      def goAwayFromBase:Option[EntityAction] = neighbours.filter(x => x.enemyPower < (current.power + x.power) * attackPositionMultiplier)
        .maxByOption(x => Vec2Int(0, 0).distanceTo(x.pos))
        .map(ri => EntityAction(Some(MoveAction(ri.center, true, !ri.myUnits.values.flatten.exists(x => isBuilding(x.entityType)))), None, None, None))


      def fleeFromDanger:EntityAction = {
        val reg = neighbours.filter(x => x.enemyPower < (current.power + x.power) * attackPositionMultiplier)
          .maxByOption(_.reward)       match {
          case Some(res) => res
          case None => neighbours.minBy(_.enemyPower)
        }
         EntityAction(Some(MoveAction(reg.center, true, false)), None, None, None)
      }




      current.recommendedAction =
        if(current.power >= current.enemyPower * holdPositionMultiplier){ //we win or tie in region
          if(current.reward >= minValuableReward) { //we interested in region
            Some(Stay)
          } else { //wanna leave region
            None //goToNeighbourToHelp//.getOrElse(goAwayFromBase.getOrElse(stayHereAndAttack))
          }
        } else { //weWannaLeaveRegion
          Some(Flee)
//          Some(goToNeighbourToHelp.getOrElse(fleeFromDanger))
        }



    }

    def macroMovement(e:Entity):EntityAction = {
      val best = regionInfos.flatten.sortBy(- _.reward)
      val dir = new Random(e.id).nextInt(5)
//      val xdir = dir % 2
//      val ydir = (dir / 2) % 2
//      val dest = Vec2Int(xdir * g.pw.mapSize - 1, ydir * g.pw.mapSize - 1)
//      EntityAction(Some(MoveAction(dest, false, false)), None, None, None)
      val target = best(dir)
      EntityAction(Some(MoveAction(target.pos, false, false)), None, None, None)

    }

    (g.my(RANGED_UNIT) ++ g.my(MELEE_UNIT)).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction match {
      case Some(Stay) =>
        val targets = if(region(x.position.x, x.position.y).enemyUnits.contains(RANGED_UNIT)) Seq(RANGED_UNIT, MELEE_UNIT) else Seq()
        EntityAction(None, None, Some(AttackAction(None, Some(AutoAttack(regionSize,targets)))), None)
      case Some(Flee) =>
        val neighbourCords = rectNeighbours(x.position.x / regionSize, x.position.y / regionSize, 1, regions, regions)
        val neighbours = neighbourCords.map{ case (i, i1) => regionInfos(i)(i1)}
        val reg = neighbours.filter(x => x.enemyPower < (region(x.pos.x, x.pos.y).power + x.power) * attackPositionMultiplier)
          .maxByOption(_.reward) match {
          case Some(res) => res
          case None => neighbours.minBy(_.enemyPower)
        }
        EntityAction(Some(MoveAction(reg.center, true, false)), None, None, None)
      case None =>
         macroMovement(x)
    })).toMap

//    g.my(RANGED_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap ++
//    g.my(MELEE_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap
  }
}

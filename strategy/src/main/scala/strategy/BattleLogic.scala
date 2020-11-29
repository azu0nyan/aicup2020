package strategy

import model._
import model.EntityType._

object BattleLogic extends StrategyPart {


  class RegionInfo(
                  var pos:(Int, Int),
                  var size:Int,
                  var reward:Int,
                  var danger:Int,
                  var power:Int,
                  var movingPower:Int,
                  var moveTo:(Int,Int) = (0, 0 )
                  ){
//    def bonusDelta:Int = reward - danger

//    def deltaWith(ot:RegionInfo):Int = bonusDelta - ot.bonusDelta
  }

  val regionSize:Int = 10


  override def getActions(implicit g: GameInfo): ActionMap = {
    val regions = g.pw.mapSize / regionSize
    val sq:Array[Array[RegionInfo]] = Array.tabulate(regions, regions)((x, y) => new RegionInfo((x * regionSize, y * regionSize), regionSize, 0, 0))
    g.enemies.flatMap(g.entitiesByPlayer).foreach{ e =>
      val (ecx, ecy) = (e.position.x / regionSize, e.position.y / regionSize)
      val (reward, danger) = e.entityType match {
        case BUILDER_UNIT => (10, 1)
        case BUILDER_BASE => (100, 0)
        case WALL => (1, 0)
        case HOUSE => (200, 0)
        case MELEE_BASE => (50, 0)
        case MELEE_UNIT => (20, 20)
        case RANGED_BASE => (200, 0)
        case RANGED_UNIT => (30, 30)
        case TURRET => (50, 100)
        case RESOURCE => (0, 0)
      }
      sq(ecx)(ecy).danger += danger
      sq(ecx)(ecy).reward += reward
      val (power, movingPower) = e.entityType match {
        case BUILDER_UNIT => (1, 0)
        case BUILDER_BASE => (0, 0)
        case WALL => (0, 0)
        case HOUSE => (0, 0)
        case MELEE_BASE => (0, 0)
        case MELEE_UNIT => (20, 20)
        case RANGED_BASE => (0, 0)
        case RANGED_UNIT => (30, 30)
        case TURRET => (50, 0)
        case RESOURCE => (0, 0)
      }
    }

    for(i<- 0 until regions; j <- 0 until regions){
//      val currentReward = sq(i)(j).reward
//
//      rectNeighbours(i, j, regions, regions).map { case (i, i1) => sq(i)(i1)}.maxBy(r => )
    }


  }
}

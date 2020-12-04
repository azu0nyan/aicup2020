package strategy

import model.EntityType._
import model._

import scala.collection.mutable

object ProductionLogic extends StrategyPart {


  val minWorkers: Int = 4

  val workersToArmyRatio: Double = 0.5

  val rangedToArmyRatio: Double = 0.4


  //  def targetRanged(implicit g: GameInfo):Int = g.a

  def canProduce(u: EntityType)(implicit g: GameInfo): Boolean =
    g.populationFree >= u.populationUse && g.myResources >= g.unitCost(u) && g.my(unitToBuilderBase(u)).exists(_.active)


  def findFreeCellAround(e: Entity)(implicit g: GameInfo): Option[(Int, Int)] = rectNeighbours(e.position.x, e.position.y, e.entityType.size)
    .find(g.cellToEntity(_).isEmpty)


  def produce(unitType: EntityType)(implicit g: GameInfo): Option[(Int, EntityAction)] =
    g.my(unitToBuilderBase(unitType)).find(_.active).flatMap { building =>
      findFreeCellAround(building).map { producePosition =>
        println(s"Producing $unitType at $producePosition")
        g.populationFree -= unitType.populationUse //Update resource status
        g.myResources -= g.unitCost(unitType)
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

    g.myProductionBuildings.foreach( x => res += (x.id -> EntityAction(None, None, None, None)))

    if (canProduce(BUILDER_UNIT) && (g.my(BUILDER_UNIT).size < minWorkers | g.armyTotal * workersToArmyRatio > g.my(BUILDER_UNIT).size )) {
      produce(BUILDER_UNIT).foreach(x => res += x)
    }

    if (g.armyTotal * rangedToArmyRatio >= g.my(RANGED_UNIT).size) {
      if (canProduce(RANGED_UNIT)) produce(RANGED_UNIT).foreach(x => res += x)
    } else {
      if (canProduce(MELEE_UNIT)) produce(MELEE_UNIT).foreach(x => res += x)
    }

  /*  if (canProduce(BUILDER_UNIT) &&  g.armyTotal * workersToArmyRatio > g.my(BUILDER_UNIT).size) {
      produce(BUILDER_UNIT).foreach(x => res += x)
    }*/
    res.toMap
  }
}

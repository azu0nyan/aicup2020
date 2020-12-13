package strategy

import model.EntityType.BUILDER_UNIT
import model.{Entity, EntityAction, MoveAction, RepairAction}

object ActivateRepairLogic extends StrategyPart {

  val maxRepairDistance = 10

  val maxWorkerPerHouse = 4

  override def getActions(implicit g: GameInfo): ActionMap = {
    g.needRepairBUildings.flatMap { b =>
//      val workersClose = g.nonReservedWorkers.filter(w => w.position.distanceTo(b.position) < maxRepairDistance)
      val workersClose = g.findNClosestReachableToBuilding(b.position.x, b.position.y, b.entityType.size, maxWorkerPerHouse,
        e => !e.isEnemy && e.entityType == BUILDER_UNIT && g.nonReservedWorkers.contains(e),        maxRepairDistance, true)
//      println(workersClose)

        g.nonReservedWorkers.filter(w => w.position.distanceTo(b.position) < maxRepairDistance)
      val borderCells = rectNeighbours(b.position.x, b.position.y, b.entityType.size, g.mapSize, g.mapSize)
        .filter(c => g.entitiesMap(c.x)(c.y).isEmpty || isUnit(g.entitiesMap(c.x)(c.y).get.entityType))

      val maxWorkers = math.min(borderCells.size, maxWorkerPerHouse)

      val distance:Entity => Int = e => distanceFromSquare(b.position.toProd, b.entityType.size, e.position.toProd)

      val workers =
        if (workersClose.nonEmpty) workersClose.toSeq.sortBy(distance).take(maxWorkers)
        else g.nonReservedWorkers.toSeq.sortBy(distance).take(maxWorkers)




      workers.flatMap { w =>
        g.findClosestReachable(w.position.x, w.position.y, e => e == b, maxRepairDistance, avoidUnits = true).map {
          case (_, Seq()) => g.repair(w, b)
          case (_, x) =>
            g.paths += x  //debug
            g.move(w, x.head)
        }


      }
    }.toMap
  }
}

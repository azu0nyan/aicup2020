package strategy

import model.{EntityAction, MoveAction, RepairAction}

object ActivateRepairLogic extends StrategyPart {

  val maxRepairDistance = 12

  val maxWorkerPerHouse = 4

  override def getActions(implicit g: GameInfo): ActionMap = {
    g.needRepairBUildings.flatMap { b =>
      val workersClose = g.nonReservedWorkers.filter(w => w.position.distanceTo(b.position) < maxRepairDistance)
      val borderCells = rectNeighbours(b.position.x, b.position.y, b.entityType.size, g.mapSize, g.mapSize)
        .filter(c => g.entitiesMap(c.x)(c.y).isEmpty || isUnit(g.entitiesMap(c.x)(c.y).get.entityType))

      val maxWorkers = math.min(borderCells.size, maxWorkerPerHouse)

      val workers =
        (if (workersClose.nonEmpty) workersClose.toSeq.sortBy(_.position.distanceTo(b.position)).take(maxWorkers)
        else g.nonReservedWorkers.toSeq.sortBy(_.position.distanceTo(b.position)).take(maxWorkers))


      workers.flatMap { w =>
        g.findClosestReachable(w.position.x, w.position.y, e => e == b, maxRepairDistance, avoidUnits = true).map {
          case (_, Seq()) => g.repair(w, b)
          case (_, x) => g.move(w, x.head)
        }


      }
    }.toMap
  }
}

package strategy

import model.{EntityAction, MoveAction, RepairAction}

object ActivateRepairLogic extends StrategyPart {

  val maxRepairDistance = 12

  val maxWorkerPerHouse = 3

  override def getActions(implicit g: GameInfo): ActionMap = {
    g.needRepairBUildings.flatMap{ b =>
      val workersClose = g.nonReservedWorkers.filter( w => w.position.distanceTo(b.position) < maxRepairDistance)

      (if(workersClose.nonEmpty) workersClose.toSeq.sortBy(_.position.distanceTo(b.position)).take(maxWorkerPerHouse)
      else g.nonReservedWorkers.toSeq.sortBy(_.position.distanceTo(b.position)).take(maxWorkerPerHouse))
        .map(w => (w, rectNeighbours(b.position.x, b.position.y, b.entityType.size).minBy(pos => w.position.distanceTo(pos)))).map{
        case (entity, pos) =>
          g.reservedUnits += entity
          (entity.id, EntityAction(Some(MoveAction(pos, true, false)),None, None, Some(RepairAction(b.id))))
      }
    }
  }.toMap
}

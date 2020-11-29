package strategy

import model.{EntityAction, MoveAction, RepairAction}

object ActivateRepairLogic extends StrategyPart {
  override def getActions(implicit g: GameInfo): ActionMap = {
    g.nonActiveBuildings.flatMap{ b =>
      g.nonReservedWorkers
        .map(w => (w, rectNeighbours(b.position.x, b.position.y, b.entityType.size).minBy(pos => w.position.distanceTo(pos)))).map{
        case (entity, pos) =>
          g.reservedWorkers = g.reservedWorkers :+ entity
          (entity.id, EntityAction(Some(MoveAction(pos, true, false)),None, None, Some(RepairAction(b.id))))
      }

    }
  }.toMap
}

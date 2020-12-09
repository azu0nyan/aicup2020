package strategy

import model.EntityType._
import model._

object MiningLogic extends StrategyPart {

  def closestMine(point: (Int, Int))(implicit g: GameInfo): Option[Entity] =
    g.minableResource.filter(e => g.region(e.position.toProd).danger9 <= 15).minByOption(r => r.position.distanceTo(point))

  override def getActions(implicit g: GameInfo): ActionMap = {
    var res : Map[Int, EntityAction] = Map()

     g.minableResource.foreach{r =>
      rectNeighboursV(r.position, 1).map(g.entitiesMap(_)).find {
        case Some(e) if e.playerId.contains(g.me.id) && e.entityType == BUILDER_UNIT && !g.reservedUnits.contains(e) => true
        case _ => false
      }.flatten.foreach{
        e =>
          g.reservedUnits += e
          g.minableResource -= r
          res += (e.id -> EntityAction(None, None, Some(AttackAction(Some(r.id), None)), None ))
      }
    }

    if (g.resources.nonEmpty) g.nonReservedWorkers.foreach { worker =>
      closestMine(worker.position).foreach { cl =>
        g.minableResource -= cl
        g.reservedUnits += worker
        res += (worker.id -> EntityAction(
          Some(MoveAction(cl.position, true, false)),
          None,
          Some(AttackAction(Some(cl.id), None)),
          None
        ))
      }
    }
    res


  }
}

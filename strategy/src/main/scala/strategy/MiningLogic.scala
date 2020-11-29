package strategy

import model.EntityType._
import model._

object MiningLogic extends StrategyPart {

  def closestMine(point: (Int, Int))(implicit g: GameInfo): Entity = g.resources.minBy(r => r.position.distanceTo(point))

  override def getActions(implicit g: GameInfo): ActionMap = {
    if (g.resources.nonEmpty) g.nonReservedWorkers.map { u =>
      val cl = closestMine(u.position)
      (u.id, EntityAction(
        Some(MoveAction(cl.position, true, false)),
        None,
        Some(AttackAction(Some(cl.id), None )),
        None
      ))
    }.toMap else Map()


  }
}

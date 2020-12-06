package strategy

import model.EntityType._
import model._

object FirstTickStrategy {

  def getActions(pw:PlayerView):Map[Int, EntityAction] = {
    pw.entities.filter(_.entityType == BUILDER_UNIT).filter(_.playerId.contains(pw.myId))
      .map(e => (e.id, EntityAction(None, None , Some(AttackAction(None, Some(AutoAttack(10, Seq(RESOURCE))))), None))) toMap
  }

}

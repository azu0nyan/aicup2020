package strategy

import model.{AttackAction, AutoAttack, EntityAction}

object SendWorkersAttackAtEnd extends StrategyPart {
  override def getActions(implicit g: GameInfo): ActionMap = {
    if(g.macroState.playerPowers.filter(_._1 != g.me).forall{case (pl, pow) => pow * 3 <= g.myWorkers.size}){
      g.nonReservedWorkers.map(w => (w.id, EntityAction(None, None, Some(AttackAction(None ,Some(AutoAttack(100, Seq())))), None))).toMap
    } else Map()

  }
}

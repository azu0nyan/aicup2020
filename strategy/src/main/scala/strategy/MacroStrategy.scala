package strategy

import model.EntityAction

object MacroStrategy extends StrategyPart {

  override def getActions(implicit g: GameInfo): ActionMap = {
    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()


    res = combineOnePr(res, ActivateRepairLogic.getActions)
    res = combineOnePr(res, ProductionLogic.getActions)
    res = combineOnePr(res, MiningLogic.getActions)
    res = combineOnePr(MinersAvoidDamageLogic.getActions, res)
    res = combineOnePr(res, BattleLogic.getActions)
    res
  }
}

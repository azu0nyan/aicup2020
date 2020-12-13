package strategy

import model.EntityAction

object MacroStrategy extends StrategyPart {




  override def getActions(implicit g: GameInfo): ActionMap = {

    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()

    def doStep(code : => Unit ): Long ={
      val start = System.currentTimeMillis()
      code
      System.currentTimeMillis() - start
    }


    val tTime = doStep{res = combineOnePr(res, TacticsLogic.getActions)}
    val aTime = doStep{res = combineOnePr(res, ActivateRepairLogic.getActions)}
    val madTime = doStep{res = combineOnePr(res, MinersAvoidDamageLogic.getActions)}
    val bTime = doStep{res = combineOnePr(res, BuildingLogic.getActions)}
    val mTime = doStep{res = combineOnePr(res, MiningLogic.getActions)}
    val pTime = doStep{res = combineOnePr(res, ProductionLogic.getActions)}
    val baTime = doStep{res = combineOnePr(res, BattleLogic.getActions)}

    println(s"tact $tTime act $aTime mavoid $madTime build $bTime mine $mTime prod $pTime bat $baTime")
    res
  }
}

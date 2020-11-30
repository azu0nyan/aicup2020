import model._
import model.EntityType._
import strategy._

import scala.collection.mutable

class MyStrategy {


  implicit private [this] var gameInfo:GameInfo = _

//  def productionLogic():Map[Int, EntityAction] =
  var totalTime = 0L
  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    val tickStart = System.currentTimeMillis()
    println(s"Tick ${playerView.currentTick} strarted")
    strategy.entityProperties = playerView.entityProperties
    gameInfo = new strategy.GameInfo(playerView)
    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()

    res = merge(res, BuildingLogic.getActions)
    res = merge(res, ActivateRepairLogic.getActions)
    res = merge(res, ProductionLogic.getActions)
    res = merge(res, MiningLogic.getActions)
    res = merge(res, BattleLogic.getActions)


//    debugInterface.get.
//    debugInterface.foreach(x => x.send())
//    println(res)
    val t = System.currentTimeMillis() - tickStart
    totalTime += t
    println(f"Tick ${playerView.currentTick} ended ${t} ms. total time $totalTime avg time ${totalTime.toDouble /(playerView.currentTick + 1) }%.1f")
    model.Action(res)
  }


  def debugUpdate(playerView: model.PlayerView, debugInterface: DebugInterface) {
    debugInterface.send(model.DebugCommand.Clear())
    debugInterface.getState()
  }
}
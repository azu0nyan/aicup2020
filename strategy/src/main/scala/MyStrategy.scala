import model._
import model.EntityType._
import strategy._

import scala.collection.mutable

class MyStrategy {


  implicit private [this] var gameInfo:GameInfo = _

//  def productionLogic():Map[Int, EntityAction] =

  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    strategy.entityProperties = playerView.entityProperties
    gameInfo = new strategy.GameInfo(playerView)
    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()

    res = merge(res, BuildingLogic.getActions)
    res = merge(res, ActivateRepairLogic.getActions)
    res = merge(res, ProductionLogic.getActions)
    res = merge(res, MiningLogic.getActions)


//    debugInterface.get.
//    debugInterface.foreach(x => x.send())
//    println(res)
    model.Action(res.toMap)
  }


  def debugUpdate(playerView: model.PlayerView, debugInterface: DebugInterface) {
    debugInterface.send(model.DebugCommand.Clear())
    debugInterface.getState()
  }
}
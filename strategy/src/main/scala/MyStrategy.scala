import model.DebugCommand.Add
import model.DebugData.PlacedText
import model._
import model.EntityType._
import strategy._

import scala.collection.mutable

class MyStrategy {


  implicit private [this] var gameInfo:GameInfo = null
//    new GameInfo(PlayerView(0, 0, false, Map(), 0, 0,0,Seq(),Seq()))

//  def productionLogic():Map[Int, EntityAction] =
  var totalTime = 0L
  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    val tickStart = System.currentTimeMillis()
    println(s"Tick ${playerView.currentTick} strarted")

    if(playerView.currentTick == 0){
      GameInfo.firstRead(playerView)
    }
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
    if (gameInfo != null) {
      println("Sending debug")
      for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.powerMap(i)(j) != 0) {
        debugInterface.send(Add(PlacedText(
          ColoredVertex(Some(Vec2Float(i, j)), Vec2Float(0, 0), Color(.1f, .7f, .2f, 1f)),
          gameInfo.powerMap(i)(j).toString,
          0f,
          10f,
        )))
      }
      for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.dangerMap(i)(j) != 0) {
        debugInterface.send(Add(PlacedText(
          ColoredVertex(Some(Vec2Float(i, j + .5f)), Vec2Float(0, 0), Color(.9f, .1f, .2f, 1f)),
          gameInfo.dangerMap(i)(j).toString,
          0f,
          10f,
        )))
      }
      debugInterface.getState()
    }
  }
}
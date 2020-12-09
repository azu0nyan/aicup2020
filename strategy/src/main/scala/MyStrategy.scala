import model.DebugCommand.Add
import model.DebugData.PlacedText
import model._
import model.EntityType._
import strategy._
import visualiser.{Drawing, DrawingUtils, Visualiser, WorldStateDrawer}

import java.awt
import java.awt.event.KeyEvent
import scala.collection.mutable

class MyStrategy {


  implicit private[this] var gameInfo: GameInfo = null



  //    new GameInfo(PlayerView(0, 0, false, Map(), 0, 0,0,Seq(),Seq()))

  //  def productionLogic():Map[Int, EntityAction] =
  var totalTime = 0L

  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {


    val tickStart = System.currentTimeMillis()
    println(s"Tick ${playerView.currentTick} started players: ${playerView.players.size}")
    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()
    try {

      if (playerView.currentTick == 0 | !GameInfo.firstReadHappened) {
        GameInfo.firstRead(playerView)
        GameInfo.firstReadHappened = true
        gameInfo = new strategy.GameInfo(playerView)
        res = FirstTickStrategy.getActions(playerView)
      } else {
        gameInfo = new strategy.GameInfo(playerView)
        res = MacroStrategy.getActions
      }



    } catch {
      case t: Throwable =>
        println(s"ERROR $t")
        System.err.println("caught")
        t.printStackTrace()
    }

    val t = System.currentTimeMillis() - tickStart
    totalTime += t
    println(f"Tick ${playerView.currentTick} ended ${t} ms. total time $totalTime avg time ${totalTime.toDouble / (playerView.currentTick + 1)}%.1f")

    Visualiser.gameInfo = gameInfo
    Visualiser.lastActions = res

    model.Action(res)
  }


  val showVisualiser = false
  var visualiserShown = false
  var showVisualiserFollowCamera = false
//  var debugLastAc

  def debugUpdate(playerView: model.PlayerView, debugInterface: DebugInterface) {
    debugInterface.send(model.DebugCommand.Clear())

    if (playerView.currentTick == 0 && showVisualiser) {
      Visualiser.initDebugDrawing()
      visualiserShown = true
    }

    if (showVisualiser && gameInfo != null) {

      if (showVisualiserFollowCamera) {
        Drawing.camera.lookAt(debugInterface.getState().camera.center * Vec2Float(1, -1) - Vec2Float(0, 80))
      }

      if (debugInterface.getState().pressedKeys.contains("U")) {
        showVisualiserFollowCamera = false
      }
      if (debugInterface.getState().pressedKeys.contains("F")) {
        showVisualiserFollowCamera = true
      }

      //      debugInterface.getState(debugInterface.getState())
    }

    debugInterface.getState()
  }
}

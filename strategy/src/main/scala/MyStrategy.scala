import model.DebugCommand.Add
import model.DebugData.PlacedText
import model._
import model.EntityType._
import strategy._
import visualiser.{Drawing, DrawingUtils, WorldStateDrawer}

import java.awt.event.KeyEvent
import scala.collection.mutable

class MyStrategy {


  implicit private[this] var gameInfo: GameInfo = null

  //    new GameInfo(PlayerView(0, 0, false, Map(), 0, 0,0,Seq(),Seq()))

  //  def productionLogic():Map[Int, EntityAction] =
  var totalTime = 0L

  val showVisualiser = false
  var showVisualiserFollowCamera = false

  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    if (playerView.currentTick == 0 && showVisualiser) {
      Drawing.startDrawingThread(Vec2Int(1920, 1080), false, new visualiser.Camera(initialZoom = 47, initialLookAt = Vec2Float(40, -130), controlsEnabled = true))
      Drawing.addDrawable(new WorldStateDrawer(() => gameInfo))
      Drawing.addKeyBinding(KeyEvent.VK_ESCAPE, () => Drawing.stopDrawing())

      val danger = Drawing.addDrawer(g => {
        if (gameInfo != null) {
          for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.dangerMap(i)(j) != 0) {
            DrawingUtils.drawText(gameInfo.dangerMap(i)(j).toString, Vec2Float(i +.1f, j +.2f ), g, 10, false,
              new java.awt.Color(255, 0, 0))
          }
        }
      })

      val power = Drawing.addDrawer(g => {
        if (gameInfo != null) {
          for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.powerMap(i)(j) != 0) {
            DrawingUtils.drawText(gameInfo.powerMap(i)(j).toString, Vec2Float(i +.1f, j +.7f ), g, 10, false,
              new java.awt.Color(0, 255, 0))
          }
        }
      })
    }

    val tickStart = System.currentTimeMillis()
    println(s"Tick ${playerView.currentTick} strarted")

    if (playerView.currentTick == 0) {
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
    println(f"Tick ${playerView.currentTick} ended ${t} ms. total time $totalTime avg time ${totalTime.toDouble / (playerView.currentTick + 1)}%.1f")
    if (showVisualiser) {

    }

    model.Action(res)
  }


  def debugUpdate(playerView: model.PlayerView, debugInterface: DebugInterface) {
    debugInterface.send(model.DebugCommand.Clear())
    println(debugInterface.getState().pressedKeys)
    if (showVisualiser && gameInfo != null) {

      if (showVisualiserFollowCamera) {
        Drawing.camera.lookAt(debugInterface.getState().camera.center * Vec2Float(1, -1) - Vec2Float(0, 80))
      }

      if(debugInterface.getState().pressedKeys.contains("U")){
        showVisualiserFollowCamera = false
      }
      if(debugInterface.getState().pressedKeys.contains("F")){
        showVisualiserFollowCamera = true
      }

      //      debugInterface.getState(debugInterface.getState())
    }

    debugInterface.getState()
  }
}

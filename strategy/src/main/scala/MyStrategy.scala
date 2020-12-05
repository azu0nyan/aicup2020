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

  val showVisualiser = true
  var showVisualiserFollowCamera = false

  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    if (playerView.currentTick == 0 && showVisualiser) {
      Drawing.startDrawingThread(Vec2Int(1920, 1080), false, new visualiser.Camera(initialZoom = 47, initialLookAt = Vec2Float(40, -130), controlsEnabled = true))
      Drawing.addDrawable(new WorldStateDrawer(() => gameInfo))
      Drawing.addKeyBinding(KeyEvent.VK_ESCAPE, () => Drawing.stopDrawing())

      val danger = Drawing.addDrawer(g => {
        if (gameInfo != null) {
          for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.dangerMap(i)(j) != 0) {
            DrawingUtils.drawText(gameInfo.dangerMap(i)(j).toString, Vec2Float(i + .1f, j + .2f), g, 10, false,
              new java.awt.Color(255, 0, 0))
          }
        }
      })

      val power = Drawing.addDrawer(g => {
        if (gameInfo != null) {
          for (i <- 0 until playerView.mapSize; j <- 0 until playerView.mapSize if gameInfo.powerMap(i)(j) != 0) {
            DrawingUtils.drawText(gameInfo.powerMap(i)(j).toString, Vec2Float(i + .1f, j + .7f), g, 10, false,
              new java.awt.Color(0, 255, 0))
          }
        }
      })

      val pf = Drawing.addDrawer(g => {
        if (gameInfo != null) {
          val pf = BattleLogic.pf(gameInfo)
          for (i <- 0 until gameInfo.regionInSide; j <- 0 until gameInfo.regionInSide) {
            if (pf(i)(j) != Int.MinValue) {
              DrawingUtils.drawText(pf(i)(j).toString,
                toVec2Int(gameInfo.regions(i)(j).min).toVec2Float + Vec2Float(gameInfo.regionsSize / 2, gameInfo.regionsSize * 3 / 4), g,
                20, false, java.awt.Color.BLACK
              )
            }
            val reg = gameInfo.regions(i)(j)
            if (BattleLogic.importantRegions.contains(reg)) {
              DrawingUtils.drawRect(toVec2Int(reg.min).toVec2Float, gameInfo.regionsSize, gameInfo.regionsSize, g, new java.awt.Color(255, 0, 0, 60), true)
            }
            DrawingUtils.drawText(s"D/P: ${reg.neighbours9.map(_.danger).sum} / ${reg.neighbours9.map(_.power).sum} ",
              toVec2Int(gameInfo.regions(i)(j).min).toVec2Float + Vec2Float(gameInfo.regionsSize / 2, gameInfo.regionsSize * 2 / 4), g,
              10, false, java.awt.Color.BLACK
            )
            DrawingUtils.drawText(s"D/R ${reg.neighbours9.map(_.defenceValue).sum} / ${reg.neighbours9.map(_.reward).sum}",
              toVec2Int(gameInfo.regions(i)(j).min).toVec2Float + Vec2Float(gameInfo.regionsSize / 2, gameInfo.regionsSize * 1 / 4), g,
              10, false, java.awt.Color.BLACK
            )
          }
        }
      })
    }

    val tickStart = System.currentTimeMillis()
    println(s"Tick ${playerView.currentTick} strarted")
    var res: Map[Int, EntityAction] = Map[Int, EntityAction]()
    try {

      if (!GameInfo.firstReadHappened) {
        GameInfo.firstRead(playerView)
        GameInfo.firstReadHappened = true
      }
      gameInfo = new strategy.GameInfo(playerView)


      res = merge(res, BuildingLogic.getActions)
      res = merge(res, ActivateRepairLogic.getActions)
      res = merge(res, ProductionLogic.getActions)
      res = merge(res, MiningLogic.getActions)
      res = merge(res, BattleLogic.getActions)

    } catch {
      case t: Throwable => t.printStackTrace()
    }

    val t = System.currentTimeMillis() - tickStart
    totalTime += t
    println(f"Tick ${playerView.currentTick} ended ${t} ms. total time $totalTime avg time ${totalTime.toDouble / (playerView.currentTick + 1)}%.1f")
    if (showVisualiser) {

    }

    model.Action(res)
  }


  def debugUpdate(playerView: model.PlayerView, debugInterface: DebugInterface) {
    debugInterface.send(model.DebugCommand.Clear())
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

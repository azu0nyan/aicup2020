import model.DebugCommand.Add
import model.DebugData.PlacedText
import model._
import model.EntityType._
import strategy._
import visualiser.{Drawing, DrawingUtils, WorldStateDrawer}

import java.awt
import java.awt.event.KeyEvent
import scala.collection.mutable

class MyStrategy {


  implicit private[this] var gameInfo: GameInfo = null


  var lastActions: Map[Int, EntityAction] = _
  //    new GameInfo(PlayerView(0, 0, false, Map(), 0, 0,0,Seq(),Seq()))

  //  def productionLogic():Map[Int, EntityAction] =
  var totalTime = 0L

  val showVisualiser = false
  var showVisualiserFollowCamera = false

  def initDebugDrawing(): Unit = {
    Drawing.startDrawingThread(Vec2Int(1920, 1080), false, new visualiser.Camera(initialZoom = 47, initialLookAt = Vec2Float(40, -130), controlsEnabled = true))
    Drawing.addDrawable(new WorldStateDrawer(() => gameInfo))

    val danger = Drawing.addDrawer(g => {
      if (gameInfo != null) {
        for (i <- 0 until gameInfo.pw.mapSize; j <- 0 until gameInfo.pw.mapSize if gameInfo.dangerMap(i)(j) != 0) {
          DrawingUtils.drawText(gameInfo.dangerMap(i)(j).toString, Vec2Float(i + .1f, j + .2f), g, 10, false,
            new java.awt.Color(255, 0, 0))
        }
      }
    })

    val power = Drawing.addDrawer(g => {
      if (gameInfo != null) {
        for (i <- 0 until gameInfo.pw.mapSize; j <- 0 until gameInfo.pw.mapSize if gameInfo.powerMap(i)(j) != 0) {
          DrawingUtils.drawText(gameInfo.powerMap(i)(j).toString, Vec2Float(i + .1f, j + .7f), g, 10, false,
            new java.awt.Color(0, 255, 0))
        }
      }
    })

    val pf = Drawing.addDrawer(g => {
      if (gameInfo != null) {
        val pf = BattleLogic.potentialField(gameInfo)

        for (i <- 0 until gameInfo.regionInSide; j <- 0 until gameInfo.regionInSide) {
          val reg = gameInfo.regions(i)(j)
          if (BattleLogic.importantRegions.contains(reg)) {
            DrawingUtils.drawRect(toVec2Int(reg.min).toVec2Float, gameInfo.regionsSize, gameInfo.regionsSize, g, new java.awt.Color(255, 0, 0, 60), true)
          }
          if (pf(i)(j) != Int.MinValue) {
            DrawingUtils.drawText(pf(i)(j).toString,
              toVec2Int(gameInfo.regions(i)(j).min).toVec2Float + Vec2Float(gameInfo.regionsSize / 2, gameInfo.regionsSize * 3 / 4), g,
              20, false, java.awt.Color.BLACK
            )
          }
        }
      }
    })

    val pfDetails = Drawing.addDrawer(g => {
      if (gameInfo != null) {
        for (i <- 0 until gameInfo.regionInSide; j <- 0 until gameInfo.regionInSide) {
          val reg = gameInfo.regions(i)(j)
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

    val actions = Drawing.addDrawer(g => {
      if (gameInfo != null) {
        val gi = gameInfo
        val acts = lastActions
        acts.foreach { case (i, action) =>
          gi.myEntities.find(_.id == i) match {
            case Some(myEntity) => action match {
              case EntityAction(moveAction, buildAction, attackAction, repairAction) =>
                moveAction.foreach { case MoveAction(to, _, _) =>
                  DrawingUtils.drawLine(myEntity.position.toVec2Float + Vec2Float(.5f, .5f), to.toVec2Float + Vec2Float(.5f, .5f), g, new awt.Color(0, 0, 255))
                }
                buildAction.foreach { case BuildAction(eType, pos) =>
                  DrawingUtils.drawRect(myEntity.position.toVec2Float, 1, 1, g, new awt.Color(0, 200, 0, 250), true)
                  DrawingUtils.drawRect(pos.toVec2Float, eType.size, eType.size, g, new awt.Color(0, 200, 0, 250), true)
                }
                attackAction.foreach {
                  case AttackAction(id, auto) =>
                    id.flatMap(i => gi.enemyEntities.find(_.id == i)).foreach { enemy =>
                      DrawingUtils.drawLine(myEntity.position.toVec2Float + Vec2Float(.5f, .5f), enemy.position.toVec2Float + Vec2Float(.5f, .5f), g,
                        new awt.Color(255, 0, 0))
                    }
                    auto.foreach { case AutoAttack(pathfindRange, validTargets) =>
                      val poly = Seq(Vec2Float(-pathfindRange, 0), Vec2Float(0, pathfindRange), Vec2Float(pathfindRange, 0), Vec2Float(0, -pathfindRange))
                        .map(_ + myEntity.position.toVec2Float)
                      DrawingUtils.drawPolygon(poly, g, false, new awt.Color(255, 0, 0, 120))
                    }
                }
            }

            case None =>
          }
        }
      }
    }
    )

    pfDetails.toggle()


    Drawing.addKeyBinding(KeyEvent.VK_ESCAPE, () => Drawing.stopDrawing())
    Drawing.addKeyBinding(KeyEvent.VK_1, () => danger.toggle())
    Drawing.addKeyBinding(KeyEvent.VK_2, () => power.toggle())
    Drawing.addKeyBinding(KeyEvent.VK_3, () => pf.toggle())
    Drawing.addKeyBinding(KeyEvent.VK_4, () => pfDetails.toggle())
    Drawing.addKeyBinding(KeyEvent.VK_5, () => actions.toggle())
  }

  def getAction(playerView: model.PlayerView, debugInterface: Option[DebugInterface]): model.Action = {
    if (playerView.currentTick == 0 && showVisualiser) {
      initDebugDrawing()
    }

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
    if (showVisualiser) {
      lastActions = res
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

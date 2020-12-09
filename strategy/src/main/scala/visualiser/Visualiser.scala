package visualiser

import model.{AttackAction, AutoAttack, BuildAction, EntityAction, MoveAction, Vec2Float, Vec2Int}
//import strategy.{BattleLogic, toVec2Int}
import strategy._

import java.awt
import java.awt.event.KeyEvent

object Visualiser {


  var gameInfo:GameInfo = null
  var lastActions: Map[Int, EntityAction] = Map()

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
          DrawingUtils.drawText(s"D/P: ${reg.danger9} / ${reg.power9} ",
            toVec2Int(gameInfo.regions(i)(j).min).toVec2Float + Vec2Float(gameInfo.regionsSize / 2, gameInfo.regionsSize * 2 / 4), g,
            10, false, java.awt.Color.BLACK
          )
          DrawingUtils.drawText(s"D/R ${reg.defence9} / ${reg.reward9}",
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


}

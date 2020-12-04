package visualiser

import model.Vec2Int

import java.awt.event.KeyEvent


object Drawing extends DrawingWindow {
  def stopDrawing(): Unit = {
    super.dispose()
  }



  override def startDrawingThread(size: Vec2Int, decorated: Boolean, camera:Camera = new Camera()): Unit = {

    DrawingUtils.camera = camera
    System.setProperty("java.util.logging.SimpleFormatter.format",
      "[%1$tF %1$tT] [%4$-7s] %5$s  %n")
    super.startDrawingThread(size, decorated, camera )
  }

  val  FpsCounter : EnabledDisabled = new EnabledDisabled {
    override val initialEnabled = false

    var fpsCounter = new FpsCounter
    addKeyBinding(KeyEvent.VK_F5, () => toggle() )
    log.info("FpsCounter init ...")
    onEnabled.subscribe(_ => addDrawable(fpsCounter))
    onDisabled.subscribe(_ => removeDrawable(fpsCounter))
  }




}


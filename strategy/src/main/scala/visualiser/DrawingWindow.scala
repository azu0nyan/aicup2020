package visualiser


import model.{Vec2Float, Vec2Int}
import visualiser.Event.Listener

import java.awt.event._
import java.awt.{Graphics2D, Toolkit}
import java.util.concurrent.{ConcurrentSkipListMap, CopyOnWriteArrayList}
import java.util.logging.{Level, Logger}
import javax.swing.{JFrame, WindowConstants}
import scala.jdk.CollectionConverters._

class DrawingWindow() extends JFrame {


  val log: Logger = Logger.getLogger("DrawingWindow")
  //setExtendedState(6)
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  var camera: Camera = _


  private var lastFrameEnd = 0L

  private var startTime = 0L

  private val drawableObjectsDepthMap: ConcurrentSkipListMap[Int, CopyOnWriteArrayList[DrawableUpdatable]] = new ConcurrentSkipListMap[Int, CopyOnWriteArrayList[DrawableUpdatable]]()

  private[this] var _fpsLimit: Int = 60

  private def fpsLimit: Int = _fpsLimit

  private def fpsLimit_=(value: Int): Unit = {
    _fpsLimit = value
  }

  def startDrawingThread(size: Vec2Int = Vec2Int(1920, 1080), decorated: Boolean = true, c: Camera = new Camera()): Unit = {
    camera = c
    camera.bindToWindow(this)
    setSize(size.x, size.y)
    setUndecorated(!decorated)
    setVisible(true)
    createBufferStrategy(2)
    lastFrameEnd = System.currentTimeMillis
    startTime = System.currentTimeMillis

    val drawingThread = new Thread(
      () => {
        log.info("Drawing thread started")
        while (true) {
          val dtMs = System.currentTimeMillis - lastFrameEnd
          lastFrameEnd = System.currentTimeMillis
          updateAndDraw(dtMs.toDouble / 1000.0D)
          val frameLength = System.currentTimeMillis - lastFrameEnd
          if (_fpsLimit != 0 && frameLength < (1000 / _fpsLimit).toLong) {
            Thread.sleep((1000 / _fpsLimit).toLong - frameLength)
          }
        }
      }
      , "UpdateAndDrawThread")
    drawingThread.start()
  }

  def removeDrawable(drawable: DrawableUpdatable): Unit = {
    drawableObjectsDepthMap.values().forEach(v => v.remove(drawable))
    log.log(Level.INFO, s"Object removed : $drawable   drawables total: ${drawableObjectsDepthMap.values().asScala.map(_.size()).sum}")
  }

  def addDrawer(d: Graphics2D => Unit, depth: Int = 0, visible:Boolean = true): DrawableObject = {
    val toAdd = new DrawableObject(drawMe = d, visible = visible)
    addDrawable(toAdd, depth)
    return toAdd
  }

  def addDrawable(drawable: DrawableUpdatable, depth: Int = 0): DrawableUpdatable = {
    drawableObjectsDepthMap
      .computeIfAbsent(depth, _ => new CopyOnWriteArrayList[DrawableUpdatable]())
      .add(drawable)
    log.log(Level.INFO, s"Object : $drawable with depth: $depth added, drawables total: ${drawableObjectsDepthMap.values().asScala.map(_.size()).sum}")
    return drawable
  }

  def changeDepth(drawable: DrawableUpdatable, newDepth: Int): Unit = {
    removeDrawable(drawable)
    addDrawable(drawable, newDepth)
  }

  def getAllObject: Iterable[DrawableUpdatable] = {
    drawableObjectsDepthMap.asScala.values.flatMap(layer => layer.asScala.toSeq)
  }

  private def updateAndDraw(dt: Double): Unit = {
    val bs = getBufferStrategy
    val g2d = bs.getDrawGraphics.asInstanceOf[Graphics2D]
    g2d.clearRect(0, 0, getWidth, getHeight)
    updateAndDrawObjects(g2d, dt)
    g2d.dispose()
    bs.show()
    Toolkit.getDefaultToolkit.sync()
  }

  def updateAndDrawObjects(g: Graphics2D, dt: Double): Unit = {
    drawableObjectsDepthMap.values().forEach(_.forEach(updateObject(_, dt)))
    drawableObjectsDepthMap.values().forEach(_.forEach(drawObject(_, g)))
  }

  def updateObject(drawable: DrawableUpdatable, dt: Double): Unit = {
    try {
      drawable.update(dt)
    } catch {
      case e: Exception =>
        println(drawable)
        e.printStackTrace()
    }
  }

  def drawObject(drawable: DrawableUpdatable, g: Graphics2D): Unit = {
    try {
      drawable.draw(g)
    } catch {
      case e: Exception =>
        println(drawable)
        e.printStackTrace()
    }
  }


  /**
   * @param keyCode используйте KeyEvent.VK_<КЛАВИША> чтобы указать какая клавиша нажата
   * @param pressed true событие реагирует на нажате клавиши, false на отпускание
   * @param action
   */
  def addKeyBinding(keyCode: Int, action: () => Unit, pressed: Boolean = true): KeyListener = {
    val listener = new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}

      override def keyPressed(e: KeyEvent): Unit = {
        if (pressed && e.getKeyCode == keyCode) {
          action.apply()
        }
      }

      override def keyReleased(e: KeyEvent): Unit = {
        if (!pressed && e.getKeyCode == keyCode) {
          action.apply()
        }
      }
    }
    addKeyListener(listener)
    return listener
  }


  def addMouseMovedListener(f:Vec2Float => Unit):Listener[Vec2Float] = {
    return camera.mouseInWorldPosChanged.subscribe(f)
  }
  /**
   * @param f takes mouse in world
   * @return handle used to delete binding
   */
  def addMouseLeftClickBinding(f: Vec2Float => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON1)

  /**
   * @param f takes mouse in world
   * @return handle used to delete binding
   */
  def addMouseRightClickBinding(f: Vec2Float => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON3)
  /**
   * @param f takes mouse in world
   * @return handle used to delete binding
   */
  def addMouseMiddleClickBinding(f: Vec2Float => Unit): MouseListener = addMouseClickBinding(f, MouseEvent.BUTTON2)
  /**
   * @param f takes mouse in world
   * @return handle used to delete binding
   */
  def addMouseClickBinding(f: Vec2Float => Unit, button: Int = MouseEvent.BUTTON1): MouseListener = {
    val listener = new MouseAdapter {
      override def mouseReleased(e: MouseEvent): Unit = {
        if (e.getButton == button) {
          camera.updateMousePos(e)
          f.apply(camera.mouseInWorld)
        }
      }
    }
    addMouseListener(listener)
    return listener
  }


  val mousePosDrawer:DrawableObject = addDrawer(g =>{
    DrawingUtils.drawText(f"${camera.mouseInWorld.x}%.2f ${camera.mouseInWorld.y}%.2f ", camera.mouseInWorld, g, 20, scaleFont = false)
  }, 10000, false)

}


package visualiser


import model.Vec2Float
import visualiser.Event.Event

import java.awt.event.{KeyEvent, MouseEvent, MouseMotionListener}


class Camera(initialLookAt : Vec2Float = new Vec2Float(0.0f, 0.0f),
             initialZoom: Int = 46,
             val resolution:Vec2Float = new Vec2Float(1920, 1080),
             var zooms: Seq[Float] = Seq(1.0E-8D, 2.5E-8D, 5.0E-8D, 7.5E-8D, 1.0E-7D, 2.5E-7D, 5.0E-7D, 7.5E-7D, 1.0E-6D, 2.5E-6D, 5.0E-6D, 7.5E-6D, 1.0E-5D, 2.5E-5D, 5.0E-5D, 7.5E-5D, 1.0E-4D, 2.5E-4D, 5.0E-4D, 7.5E-4D, 0.001D, 0.0025D, 0.005D, 0.0075D, 0.001D, 0.0025D, 0.005D, 0.0075D, 0.01D, 0.025D, 0.05D, 0.075D, 0.1D, 0.2D, 0.25D, 0.5D, 0.75D, 1.0D, 1.5D, 2.0D, 2.5D, 3.0D, 4.0D, 5.0D, 7.5D, 8.5d, 10.0D, 12d, 14.0, 15.0D,17.0, 20.0D,25d, 30.0D, 40.0D, 50.0D, 70.0D, 100.0D,125d, 150.0D, 200.0D, 250.0D, 300d, 350d, 400.0D, 450D, 500.0D).map(_.toFloat),
             var screenPartPerScroll: Float = 0.1f,
             var controlsEnabled: Boolean = false,
             var zoomIN: Int = KeyEvent.VK_E,
             var zoomOUT: Int = KeyEvent.VK_Q,
             var moveUP: Int = KeyEvent.VK_W,
             var moveDOWN: Int = KeyEvent.VK_S,
             var moveLEFT: Int = KeyEvent.VK_A,
             var moveRIGHT: Int = KeyEvent.VK_D,
             var invertY: Boolean = true) {

  private[this] var _cameraZoom: Int = initialZoom
  private [this] def cameraZoom: Int = _cameraZoom
  private [this] def cameraZoom_=(value: Int): Unit = {
    _cameraZoom = value
    _leftTopAngleInWorld = calcLefTopAngleInWorld()
    _zoom = calcZoom
  }

  private def calcZoom: Float = zooms(if (cameraZoom >= 0) cameraZoom % zooms.length
  else cameraZoom % zooms.length + zooms.length)

  private[this] var _zoom: Float = calcZoom
  def getZoom: Float = _zoom




  private[this] var _cameraCenterInWorld: Vec2Float = initialLookAt

  private def cameraCenterInWorld: Vec2Float = _cameraCenterInWorld

  private def calcLefTopAngleInWorld():Vec2Float = cameraCenterInWorld - ((screenResolution.normalize * (screenToWorld(screenResolution.length / 2)) *
    (if(invertY) Vec2Float(1, -1) else Vec2Float(1, 1))))

  private[this] var _leftTopAngleInWorld: Vec2Float =  calcLefTopAngleInWorld()

  def leftTopAngleInWorld: Vec2Float = _leftTopAngleInWorld

  def lookAt(value: Vec2Float) :Unit = {
    _cameraCenterInWorld = value
    _leftTopAngleInWorld = calcLefTopAngleInWorld()
  }
  cameraZoom_=(initialZoom)
  lookAt(initialLookAt)

  var mouseInWorld: Vec2Float = new Vec2Float(0.0f, 0.0f)

  var mouseOnScreen: Vec2Float = new Vec2Float(0.0f, 0.0f)


  var correspondingWindow: Option[DrawingWindow] = None

  def screenResolution: Vec2Float = resolution


  def worldToScreen(world: Float): Float = world * getZoom

  def screenToWorld(screen: Float): Float = screen / getZoom

  def worldToScreen(world: Vec2Float): Vec2Float = ((if (invertY) Vec2Float(world.x, -world.y) else world) - leftTopAngleInWorld) * getZoom

  def screenToWorld(screen: Vec2Float): Vec2Float = screen * (1f / getZoom) + leftTopAngleInWorld




  def enableControls(): Unit = {
    controlsEnabled = true
  }

  def disableControls(): Unit = {
    controlsEnabled = false
  }

  def updateMousePos(e: MouseEvent): Unit = {
    mouseOnScreen = Vec2Float(e.getX, e.getY)
    mouseInWorld = screenToWorld(mouseOnScreen);
    mouseInWorldPosChanged(mouseInWorld)
    mouseOnScreenChangedPos(mouseOnScreen)
  }

  val mouseInWorldPosChanged: Event[Vec2Float] =  Event[Vec2Float]
  val mouseOnScreenChangedPos: Event[Vec2Float] =  Event[Vec2Float]

  def bindToWindow(window: DrawingWindow): Unit = {
    correspondingWindow = Some(window)
    window.addKeyBinding(zoomIN, () => if (controlsEnabled) cameraZoom = Math.min(cameraZoom + 1, zooms.size - 1))
    window.addKeyBinding(zoomOUT, () => if (controlsEnabled) cameraZoom = Math.max(cameraZoom - 1, 0))
    window.addKeyBinding(moveUP, () => if (controlsEnabled) lookAt(cameraCenterInWorld + Vec2Float(0, -1) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveDOWN, () => if (controlsEnabled) lookAt(cameraCenterInWorld + Vec2Float(0, 1) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveLEFT, () => if (controlsEnabled) lookAt(cameraCenterInWorld + Vec2Float(-1, 0) * screenToWorld(screenResolution.length * screenPartPerScroll)))
    window.addKeyBinding(moveRIGHT, () => if (controlsEnabled) lookAt(cameraCenterInWorld + Vec2Float(1, 0) * screenToWorld(screenResolution.length * screenPartPerScroll)))

    window.addMouseMotionListener(new MouseMotionListener {
      override def mouseDragged(e: MouseEvent): Unit = {
        updateMousePos(e)
      }

      override def mouseMoved(e: MouseEvent): Unit = {
        updateMousePos(e)
      }
    })
  }


  override def toString = s"Camera($mouseInWorld, $mouseOnScreen, $cameraCenterInWorld, $cameraZoom, $controlsEnabled, $getZoom, $leftTopAngleInWorld)"
}


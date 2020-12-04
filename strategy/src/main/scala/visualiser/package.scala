import java.awt.Graphics2D

package object visualiser extends VecOps {

  trait DrawableUpdatable {
    /** вызывается в потоке рисования каждый кадр */
    def draw(g: Graphics2D) : Unit

    /** вызывается до рисования каждый кадр */
    def update(dt: Double) : Unit
  }

  class DrawableObject(
                        private var drawMe: Graphics2D => Unit = { _ => () },
                        private var updateMe: Double => Unit = { _ => () },
                        private var visible:Boolean = true
                      ) extends DrawableUpdatable with EnabledDisabled{
    if(visible)enable() else disable()
    def setDraw(d: Graphics2D => Unit): Unit = {
      drawMe = d
    }

    def setUpdate(u: Double => Unit): Unit = {
      updateMe = u
    }

    /** вызывается до рисования каждый кадр */
    override def update(dt: Double): Unit = {
      if(isEnabledNow) {
        updateMe.apply(dt)
      }
    }

    /** вызывается в потоке рисования каждый кадр */
    override def draw(g: Graphics2D): Unit = {
      if(isEnabledNow) {
        drawMe.apply(g)
      }
    }

  }


  abstract class SimpleDrawable() extends DrawableObject {
    def drawAndUpdate(g: Graphics2D, dt: Double): Unit

    var lastDt: Double = 0

    setUpdate(lastDt = _)
    setDraw(drawAndUpdate(_, lastDt))
  }
}


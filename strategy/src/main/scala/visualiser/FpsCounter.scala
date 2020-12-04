package visualiser

import java.awt.{Color, Font, Graphics2D}

class FpsCounter extends SimpleDrawable() {
  var font: Font = new Font("", 0, 15)
  var rowHeight: Int = 20
  var rowNumber: Int = 1
  var dx: Int = 30
  var dy: Int = 30
  var framesCount: Int = 0
  var lastFrames: Seq[Long] = Seq()


  override def drawAndUpdate(g: Graphics2D, dt: Double): Unit = {
    this.framesCount += 1
    val currentTime: Long = System.currentTimeMillis
    lastFrames = (lastFrames :+ currentTime).filter(e => (currentTime - e) < 1000)
    g.setColor(Color.RED)
    g.setFont(font)
    g.drawString("fps:" + this.lastFrames.size, this.dx, this.dy + this.rowNumber * this.rowHeight)
  }

  override def toString = s"FpsCounter(${lastFrames.size} fps)"
}

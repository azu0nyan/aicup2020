package visualiser

import model.Vec2Float

import java.awt.{BasicStroke, Color, Font, Graphics2D}

object DrawingUtils {

  var camera: Camera = new Camera

  def drawPolygon(p: Seq[Vec2Float], g: Graphics2D, fill: Boolean = false, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    val transformed = p.map(
      v => camera.worldToScreen(v)
    )
    val xs = transformed.map(_.x.toInt).toArray
    val ys = transformed.map(_.y.toInt).toArray
    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    if (fill) {
      g.fillPolygon(xs, ys, xs.length)
    } else {
      g.drawPolygon(xs, ys, xs.length)
    }
  }
  def drawText(text: String, where: Vec2Float, g: Graphics2D, fontSize: Double, scaleFont: Boolean = true, color: Color = Color.BLACK, fontName: String = "Lucida Console", fontType: Int = Font.PLAIN): Unit = {
    g.setColor(color)
    g.setFont(new Font(fontName, fontType, if (scaleFont) camera.worldToScreen(fontSize.toFloat).toInt else fontSize.toInt))
    g.drawString(text, camera.worldToScreen(where).x.toInt, camera.worldToScreen(where).y.toInt)
  }

  def drawCircle(where: Vec2Float, radius: Float, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, lineWidth: Int = 2): Unit = {
    val rad = camera.worldToScreen(radius).toInt
    val wh = camera.worldToScreen(where)
    g.setColor(color)
    if (fill) {
      g.fillOval(wh.x.toInt - rad, wh.y.toInt - rad, 2 * rad, 2 * rad)
    } else {
      g.setStroke(new BasicStroke(lineWidth.toFloat))
      g.drawOval(wh.x.toInt - rad, wh.y.toInt - rad, 2 * rad, 2 * rad)
    }
  }

  def drawRect(min: Vec2Float, width:Float, height:Float, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, lineWidth: Int = 2): Unit = {
    val w = camera.worldToScreen(width).toInt
    val h = camera.worldToScreen(height).toInt
    val xy = camera.worldToScreen(min)
    val ot = camera.worldToScreen(min + Vec2Float(width, height))
    val nmin = Vec2Float(math.min(xy.x, ot.x), math.min(xy.y, ot.y))
    g.setColor(color)
    if (fill) {
      g.fillRect(nmin.x toInt, nmin.y toInt, w, h )
    } else {
      g.setStroke(new BasicStroke(lineWidth.toFloat))
      g.drawRect(nmin.x toInt, nmin.y toInt, w, h )
    }
  }

  def drawPoint(where: Vec2Float, radius: Float, g: Graphics2D, color: Color = Color.BLACK, fill: Boolean = false, pointWidth: Int = 5): Unit = {
    val rad = pointWidth / 2
    val wh = camera.worldToScreen(where)
    g.setColor(color)
    if (fill) {
      g.fillOval(wh.x.toInt - rad, wh.y.toInt - rad, 2 * rad, 2 * rad)
    } else {
      g.setStroke(new BasicStroke(1))
      g.drawOval(wh.x.toInt - rad, wh.y.toInt - rad, 2 * rad, 2 * rad)
    }
  }

  def drawSegment(v1:Vec2Float, v2:Vec2Float, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    drawLine(v1, v2, g, color, lineWidth)
  }

 /* def drawArrow(from: Vec2Float, to: Vec2Float, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1, arrowHeadSize: Double = 0.5f): Unit = {
    val f = camera.worldToScreen(from)
    val t = camera.worldToScreen(to)
    val tfDelta: Vec2Float = (f - t).normalize * camera.worldToScreen(arrowHeadSize).toFloat
    val arrowEnd1 = tfDelta.rotate(EIGHT_PI) + t
    val arrowEnd2 = tfDelta.rotate(-EIGHT_PI) + t

    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    g.drawLine(arrowEnd1.x.toInt, arrowEnd1.y.toInt, t.x.toInt, t.y.toInt)
    g.drawLine(arrowEnd2.x.toInt, arrowEnd2.y.toInt, t.x.toInt, t.y.toInt)
    g.drawLine(f.x.toInt, f.y.toInt, t.x.toInt, t.y.toInt)
  }*/
  def drawLine(from: Vec2Float, to: Vec2Float, g: Graphics2D, color: Color = Color.BLACK, lineWidth: Int = 1): Unit = {
    val f = camera.worldToScreen(from)
    val t = camera.worldToScreen(to)
    g.setColor(color)
    g.setStroke(new BasicStroke(lineWidth.toFloat))
    g.drawLine(f.x.toInt, f.y.toInt, t.x.toInt, t.y.toInt)
  }
}

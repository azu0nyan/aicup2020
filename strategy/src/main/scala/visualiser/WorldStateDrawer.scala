package visualiser
import model.EntityType._
import model.EntityType._
import model.{Entity, EntityType, Vec2Float, Vec2Int}
import strategy.GameInfo
import strategy._

import java.awt.{Color, Graphics2D}


class WorldStateDrawer( gi:() => GameInfo) extends DrawableUpdatable {


  val bgColor = new Color(223,232,255)
  val myColor = new Color(123,200,255)
  val e1Color:Color = new Color(22,160,28)
  val e2Color = new Color(242,12,25)
  val e3Color = new Color(226,190, 55)
  val resourceColor = new Color(122,112,75)
  val textColor = new Color(2,2,5)

  def draw(e:Entity, color: Color)(implicit g:Graphics2D) : Unit = {

    //layer1
    e.entityType match {
      case WALL | HOUSE | BUILDER_BASE | MELEE_BASE | RANGED_BASE | TURRET=>
        DrawingUtils.drawRect(e.position.toVec2Float, e.entityType.size, e.entityType.size, g, color, true)
      case RESOURCE =>
        val offset = ((e.entityType.maxHealth - e.health) / e.entityType.maxHealth.toFloat) / 2f
        DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(offset, offset), 1f - 2* offset, 1f - 2* offset, g, color, true)
      case BUILDER_UNIT =>
        DrawingUtils.drawCircle(e.position.toVec2Float + Vec2Float(.5f, .5f), .4f, g, color, true)
      case MELEE_UNIT =>
        DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(.1f, .1f), .8f, .8f, g, color, true)
      case RANGED_UNIT =>
        DrawingUtils.drawPolygon(Seq(Vec2Float(.2f, .2f), Vec2Float(.5f, .9f), Vec2Float(.8f, .2f)).map(_ + e.position.toVec2Float), g, true, color)
    }
    //layer2
    e.entityType match {
      case WALL =>
         DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(.3f, .7f), .4f,.3f, g, textColor, true)
      case HOUSE =>
        DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(.5f, 1f), .5f,1f, g, textColor, true)
        DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(1.5f, 0f), .5f,2f, g, textColor, true)
      case BUILDER_BASE =>
        DrawingUtils.drawCircle(e.position.toVec2Float + Vec2Float(2.5f, 2.5f), 2f, g, textColor, true)
      case MELEE_BASE =>
        DrawingUtils.drawRect(e.position.toVec2Float + Vec2Float(1f, 1f), 3f,3f, g, textColor, true)
      case RANGED_BASE =>
        DrawingUtils.drawPolygon(Seq(Vec2Float(.3f, .2f), Vec2Float(.5f, .8f), Vec2Float(.7f, .2f)).map(x => (x * 5) + e.position.toVec2Float), g, true, textColor)
      case RESOURCE =>
      case TURRET =>
        DrawingUtils.drawPolygon(Seq(Vec2Float(.3f, .2f), Vec2Float(.5f, .8f), Vec2Float(.7f, .2f)).map(x => (x * 2) + e.position.toVec2Float), g, true, textColor)
      case MELEE_UNIT =>
      case RANGED_UNIT =>
      case BUILDER_UNIT =>
    }
  }

  def draw(e:Entity)(implicit g:Graphics2D) :Unit= {
    val color = e.playerId match {
      case Some(pId)  if(pId == gi().me.id) => myColor
      case Some(pId)  => gi().pw.players.filter(_.id != gi().me.id).indexWhere(_.id == pId) match {
        case 0 => e1Color
        case 1 => e2Color
        case 2 => e3Color
        case _ => resourceColor
      }
      case None =>
        resourceColor
    }
    draw(e, color)
  }


  /** вызывается в потоке рисования каждый кадр */
  override def draw(g: Graphics2D): Unit = {

    implicit val gg:Graphics2D = g
    if(gi() != null) {
      DrawingUtils.drawRect(Vec2Float(0, 0), gi().pw.mapSize, gi().pw.mapSize, g, bgColor, true)
      gi().pw.entities.foreach(draw)
    }
  }
  /** вызывается до рисования каждый кадр */
  override def update(dt: Double): Unit = {

  }
}

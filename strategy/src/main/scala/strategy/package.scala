import model.{Entity, EntityAction, EntityProperties, EntityType, Vec2Int}
import model.EntityType._

import scala.collection.mutable

package object strategy {

  type ActionMap = Map[Int, EntityAction]

  implicit def EntityTypeToProperties(e: EntityType): EntityProperties = GameInfo.entityProperties(e)

  implicit def toVec2Int(x: (Int, Int)): Vec2Int = Vec2Int(x._1, x._2)

  implicit def fromVec2Int(v: Vec2Int): (Int, Int) = (v.x, v.y)



  def rectNeighbours(x:Int, y:Int, size:Int, maxX:Int = Int.MaxValue, maxY:Int = Int.MaxValue):Seq[(Int, Int)] =
    (for(i <- 0 until size) yield Seq((x -1, y + i), (x + size, y + i), (x + i, y - 1), (x + i, y + size))).flatten
      .filter{ case (x, y) =>x >=0 && y >= 0 && x < maxX && y < maxY}

  def cellsInRange(x:Int, y:Int, size:Int):Seq[(Int, Int)] = {
    val res = mutable.Buffer[(Int, Int)]()
    for( i <- (x - size) to x; j <- 0 to size){
      res.append((i + j, y - i + j))
    }
    for( i<- (x - size + 1) to x; j <- 0 until size){
      res.append((i + j, y - i - 1 + j))
    }
    res.toSeq
  }
  def sqContains(rx:Int, ry:Int, size:Int, px:Int, py:Int):Boolean = rx <= px && ry <= py && px < rx + size && py < ry + size

  def closestTo(x:Int,y:Int, ent:Seq[Entity]) :Entity = ent.minBy(e => e.position.distanceTo((x, y)))

  implicit class VecOps(val x: Vec2Int) {
    def +(ot: Vec2Int): Vec2Int = Vec2Int(x.x + ot.x, x.y + ot.y)

    def distanceTo(ot:Vec2Int) :Int = math.abs(x.x - ot.x) + math.abs(x.y - ot.y)
  }

  def haveResourcesFor(b: EntityType)(implicit g: GameInfo): Boolean = b.initialCost <= g.myResources

  val unitToBuilderBase: Map[EntityType, EntityType] = Map(
    BUILDER_UNIT -> BUILDER_BASE,
    RANGED_UNIT -> RANGED_BASE,
    MELEE_UNIT -> MELEE_BASE,
  )

  def isProductionBuilding(entityType: EntityType): Boolean = entityType match {
    case BUILDER_BASE | MELEE_BASE | RANGED_BASE => true
    case _ => false

  }

  def isBuilding(entityType: EntityType): Boolean = entityType match {
    case BUILDER_BASE | MELEE_BASE | RANGED_BASE | TURRET | HOUSE | WALL => true
    case _ => false

  }

  def isUnit(entityType: EntityType): Boolean = entityType match {
    case BUILDER_UNIT | MELEE_UNIT | RANGED_UNIT => true
    case _ => false

  }
    def merge(one:ActionMap, two:ActionMap) :ActionMap= {
      val int = one.keySet & two.keySet
      (one.keySet | two.keySet).map(x => (x, if(int.contains(x)){
         EntityAction(
          if(one(x).moveAction.nonEmpty) one(x).moveAction else two(x).moveAction,
          if(one(x).buildAction.nonEmpty) one(x).buildAction else two(x).buildAction,
          if(one(x).attackAction.nonEmpty) one(x).attackAction else two(x).attackAction,
          if(one(x).repairAction.nonEmpty) one(x).repairAction else two(x).repairAction
        )
      } else {
        one.getOrElse(x, two(x))
      }))
    }.toMap
}

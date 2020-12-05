package strategy

import model.EntityType._
import model._

object BuildingLogic {


  val baseArea = 30
  val freePopToBuildHouse:Int = 5



  def  canBuildAt(pos: (Int, Int), size:Int)(implicit g: GameInfo): Boolean =
    (0 until size).flatMap(x => (0 until(size )).map(y => !g.cantBuildArea(pos._1 + x, pos._2 + y))).forall(x => x)

  def findCurrentTurnBuildingSpot(size:Int)(implicit g: GameInfo):Option[(Int,Int, Entity)] =
    ((0 until baseArea by 4).iterator).flatMap(x => ((0 until baseArea by 4).iterator).map(y => (x, y)))
      .filter(pos => canBuildAt(pos, size)).flatMap { buildAt =>
      g.nonReservedWorkers.find(b => rectNeighbours(b.position.x, b.position.y, b.entityType.size).exists(n => sqContains(buildAt.x, buildAt.y, size, n.x, n.y)))
        .map{e => (buildAt._1, buildAt._2, e)}
     }.to(LazyList).headOption


  def build(u: EntityType)(implicit g: GameInfo): Option[(Entity, EntityAction)] = findCurrentTurnBuildingSpot(u.size) map {
    case (x, y, e) =>
      println(s"Found spot for $u at $x $y with builder $e")
      g.reservedWorkers = g.reservedWorkers :+ e
      (e, EntityAction(None, Some(BuildAction(u, (x, y))), None, None))
  }/* orElse {
   NOne
  }*/



}

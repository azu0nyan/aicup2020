package strategy

import model.EntityType._
import model._

object BuildingLogic {


  val baseArea = 30
  val freePopToBuildHouse: Int = 5

  val houseSpots =
    Seq((0, 0), (0, 3), (0, 6), (0, 9), (0, 12), (0, 15), (0, 18), (0, 21), (11, 4), (11, 7), (11, 11))
      .flatMap {
        case (x, y) if x == y => Seq((x, x))
        case (x, y) => Seq((x, y), (y, x))
      } ++      ((5 until baseArea by 4)).flatMap(x => ((5 until baseArea by 4)).map(y => (x, y)))


  def canBuildAt(pos: (Int, Int), size: Int)(implicit g: GameInfo): Boolean =
    (0 until size).flatMap(x => (0 until (size)).map(y => !g.cantBuildArea(pos._1 + x, pos._2 + y))).forall(x => x)

  def findCurrentTurnBuildingSpotForHouse(size: Int)(implicit g: GameInfo): Option[(Int, Int, Entity)] =
    houseSpots.to(LazyList).filter(pos => canBuildAt(pos, size)).flatMap { buildAt =>
      g.nonReservedWorkers.find(b => rectNeighbours(b.position.x, b.position.y, b.entityType.size).exists(n => sqContains(buildAt.x, buildAt.y, size, n.x, n.y)))
        .map { e => (buildAt._1, buildAt._2, e) }
    }.headOption


  def build(u: EntityType)(implicit g: GameInfo): Option[(Entity, EntityAction)] = findCurrentTurnBuildingSpotForHouse(u.size) map {
    case (x, y, e) =>
      println(s"Found spot for $u at $x $y with builder $e")
      g.reservedUnits += e
      (e, EntityAction(None, Some(BuildAction(u, (x, y))), None, None))
  }
  /* orElse {
     NOne
    }*/


}

package strategy

import model.EntityType._
import model._

import scala.collection.mutable

object BuildingLogic extends StrategyPart {

  val starterBase = 25
  val staterBaseTick = 400

  val baseArea = 30
  val freePopToBuildHouse: Int = 5

  val houseSpots =
    Seq((0, 0), (0, 3), (0, 6), (0, 9), (0, 12), (0, 15), (0, 18), (0, 21), (11, 4), (11, 7), (11, 11))
      .flatMap {
        case (x, y) if x == y => Seq((x, x))
        case (x, y) => Seq((x, y), (y, x))
      }.filter(_ != (3, 0)) ++ ((5 until baseArea by 4)).flatMap(x => ((5 until baseArea by 4)).map(y => (x, y)))
  val starterSpots = houseSpots.filter{case (x, y) => x <=starterBase && y <= starterBase}

  def isHouseRequired(currentPop: Int, popMax: Int): Boolean = {
    if (popMax == 0) true
    else if (popMax == 5) currentPop >= 5
    else if (popMax == 10) currentPop >= 10
    else if (popMax == 15) currentPop >= 14
    else if (popMax == 20) currentPop >= 18
    else if (popMax == 25) currentPop >= 23
    else if (popMax == 30) currentPop >= 26
    else if (popMax == 35) currentPop >= 29
    else popMax <= currentPop + 10

  }


  def canBuildAt(pos: (Int, Int), size: Int)(implicit g: GameInfo): Boolean =
    (0 until size).flatMap(x => (0 until (size)).map(y => !g.cantBuildArea(pos._1 + x)(pos._2 + y))).forall(x => x)

  def currentTurnSpot(size: Int, spots: Seq[(Int, Int)])(implicit g: GameInfo): Option[((Int, Int), Seq[Entity])] =
    spots.filter(s => canBuildAt(s, size))
      .map(s => (s, g.nonReservedWorkers.toSeq.sortBy(w => distanceFromSquare(s, size, w.position.toProd)).take(ActivateRepairLogic.maxWorkerPerHouse)))
      .filter(_._2.nonEmpty).sortBy { case (pos, ws) => ws.map(w => distanceFromSquare(pos, size, w.position.toProd)).sum }.headOption


  /* def findCurrentTurnBuildingSpotForHouse(size: Int)(implicit g: GameInfo): Option[(Int, Int, Entity)] =
     houseSpots.to(LazyList).filter(pos => canBuildAt(pos, size)).flatMap { buildAt =>
       g.nonReservedWorkers.find(b => rectNeighbours(b.position.x, b.position.y, b.entityType.size, g.mapSize, g.mapSize).exists(n => sqContains(buildAt.x, buildAt.y, size, n.x, n.y)))
         .map { e => (buildAt._1, buildAt._2, e) }
     }.headOption*/

  def findBestBuildingSpotFor(size: Int, spots: Seq[(Int, Int)])(implicit g: GameInfo): Option[((Int, Int), Seq[Entity])] =
    spots.filter(s => canBuildAt(s, size))
      .map(s => (s, g.nonReservedWorkers.toSeq
        .sortBy(w => distanceFromSquare(s, size, w.position.toProd))
        .take(ActivateRepairLogic.maxWorkerPerHouse))
      ).minByOption { case (s, workers) => workers.map(w => distanceFromSquare(s, size, w.position.toProd)).sum }


  def build(u: EntityType)(implicit g: GameInfo): Option[(Int, EntityAction)] = {
    val spots = if(g.pw.currentTick > staterBaseTick) houseSpots else starterSpots
    currentTurnSpot(u.size, spots).orElse(findBestBuildingSpotFor(u.size, spots)).flatMap {
      case (pos, Seq()) => None
      case (pos, ws) => if (distanceFromSquare(pos, u.size, ws.head.position.toProd) == 0) {
        Some(g.build(pos, ws.head, u))
      } else {
        Some(g.move(ws.head, Vec2Int(pos.x, pos.y)))
      }
    }
  }


  /*findCurrentTurnBuildingSpotForHouse(u.size) map {
    case (x, y, e) =>
      println(s"Found spot for $u at $x $y with builder $e")
      g.reservedUnits += e
      g.myMinerals -= HOUSE.buildScore
      rectArea(x, y, u.size, u.size, g.mapSize, g.mapSize).foreach(c => g.reservedForMovementCells += c)
      (e, EntityAction(None, Some(BuildAction(u, (x, y))), None, None))
  }*/
  /* orElse {
     NOne
    }*/


  override def getActions(implicit g: GameInfo): ActionMap = {

    val res: mutable.Map[Int, EntityAction] = mutable.Map[Int, EntityAction]()


    if (g.my(BUILDER_BASE).isEmpty && g.myMinerals >= BUILDER_BASE.initialCost) {
      println(s"Trying to build BUILDER_BASE")
      BuildingLogic.build(BUILDER_BASE) match {
        case Some(command) =>
          res += command
        case None =>
      }
    }

    if (g.my(RANGED_BASE).isEmpty && g.myMinerals >= RANGED_BASE.initialCost) {
      println(s"Trying to build RANGED_BASE")
      BuildingLogic.build(RANGED_BASE) match {
        case Some(command) =>
          res += command
        case None =>
      }
    }

    /*if (g.my(MELEE_BASE).isEmpty && g.myMinerals >= MELEE_BASE.initialCost) {
      println(s"Trying to build MELEE_BASE")
      BuildingLogic.build(MELEE_BASE) match {
        case Some(command) =>
          res += command
        case None =>
      }
    }*/


    val housesReq = isHouseRequired(g.populationUse, g.populationMaxWithNonactive)
    if (housesReq && g.myMinerals >= HOUSE.initialCost) {
      println(s"Trying to build house")
      BuildingLogic.build(HOUSE) match {
        case Some(command) =>
          res += command
        case None =>
      }
    }


    res.toMap
  }


}

package strategy

import helpers.BinHeap
import model.Entity

import scala.collection.mutable

object Pathfinding {
  /** maxDistance >= 1 */
  def findClosestReachable(x: Int, y: Int, filter: Entity => Boolean, maxDistance: Int, avoidUnits: Boolean = false)
                          (implicit g: GameInfo): Option[(Entity, Seq[(Int, Int)])] = {
    val visited: mutable.Set[(Int, Int)] = mutable.Set()
    val toVisit: mutable.Queue[(Int, Int)] = mutable.Queue()
    val cameFrom: mutable.Map[(Int, Int), (Int, Int)] = mutable.Map()
    toVisit += ((x, y))
    visited += ((x, y))

    //cant start path from inaccessible cell
    val (canGo, cantGo) = rectNeighbours(x, y, 1, g.mapSize, g.mapSize).partition(to => g.canMoveToNextTurn((x, y), to))
    toVisit ++= canGo
    cameFrom ++= canGo.map(c => c -> (x, y))
    visited ++= canGo
    visited ++= cantGo

    /** without first node* */
    def reconstructPath(to: (Int, Int)): Seq[(Int, Int)] = cameFrom.get(to) match {
      case None => Seq()
      case Some(from) => to +: reconstructPath(from)
    }

    while (toVisit.nonEmpty) {
      val (curx, cury) = toVisit.dequeue()
      val target = rectNeighbours(curx, cury, 1, g.mapSize, g.mapSize)
        .flatMap { case (x, y) => g.entitiesMap(x, y) }
        .find(e => filter(e))

      if (target.nonEmpty) {
        return Some((target.get, reconstructPath((curx, cury)).reverse))
      } else {
        rectNeighbours(curx, cury, 1, g.mapSize, g.mapSize).foreach {
          neigh =>
            if (distance(neigh, (x, y)) <= maxDistance && potentiallyWalkable(neigh, avoidUnits, avoidBuildings = true) && !visited.contains(neigh)) {
              cameFrom(neigh) = (curx, cury)
              visited += neigh
              toVisit += neigh
            }
        }
      }
    }
    return None

  }
  def potentiallyWalkable(xy: (Int, Int), avoidUnits: Boolean, avoidBuildings:Boolean)(implicit g: GameInfo): Boolean =
    g.entitiesMap(xy).isEmpty || (!avoidUnits && isUnit(g.entitiesMap(xy).get.entityType)) || (!avoidBuildings && isBuilding(g.entitiesMap(xy).get.entityType))

  def shortestPath(
                    from: (Int, Int), to: (Int, Int), avoidUnits: Boolean = true, avoidBuildings:Boolean = true, maxNodes:Int = 1000
                  )(implicit g: GameInfo): Option[Seq[(Int, Int)]] = {

    val nodeHeuristic: (Int, Int) => Int = (x, y) => distance((x, y), to)

    val knownBest: mutable.Map[(Int, Int), Int] = mutable.Map()
    knownBest(from) = 0
    // For node n, quenedBestGuesses[n] := knownBest[n] + h(n). quenedBestGuesses[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n
    val quenedBestGuesses: mutable.Map[(Int, Int), Int] = mutable.Map()
    quenedBestGuesses(from) = nodeHeuristic(from.x, from.y)

    val openQueue: BinHeap[(Int, Int)] = new BinHeap[(Int, Int)]()(Ordering.by((n: (Int, Int)) => quenedBestGuesses(n)))

    val cameFrom: mutable.Map[(Int, Int), (Int, Int)] = mutable.Map()
    // val cameBy: mutable.Map[(Int, Int), EdgeData] = mutable.Map()
    def reconstructPath(): Seq[(Int, Int)] = {

      val path: mutable.Buffer[(Int, Int)] = mutable.Buffer()
      var currentNode = to
      while (currentNode != from) {
        path += currentNode
        currentNode = cameFrom(currentNode)
      }
      //      println(cameFrom.size, path.size)
      path.toSeq.reverse
    }
    //

    openQueue.add(from)
    while (openQueue.nonEmpty) {
      val current = openQueue.poll()
      if (current == to) {
        return Some(reconstructPath())
      } else {
        val curNodeScore = knownBest(current)
        rectNeighbours(current.x, current.y, 1, g.mapSize, g.mapSize).filter(c => potentiallyWalkable(c, avoidUnits, avoidBuildings)).foreach { neigh =>
          val costWithCurrentEdge = curNodeScore + 1 // + cost
          // we encountered `toNode` first time || found better way, `>` filters paths with same cost
          if (!knownBest.contains(neigh) || knownBest(neigh) > costWithCurrentEdge) {
            knownBest(neigh) = costWithCurrentEdge
            quenedBestGuesses(neigh) = costWithCurrentEdge + nodeHeuristic(neigh.x, neigh.y)
            cameFrom(neigh) = current
            if (openQueue.contains(neigh)) openQueue.onOrderingChangedFor(neigh)
            else openQueue.add(neigh)
          }
        }
      }
    }
    //    println(cameFrom.size)
    return None

  }


  def findNClosestReachableToBuilding(x: Int, y: Int, size: Int, count:Int,  filter: Entity => Boolean, maxDistance: Int, avoidUnits: Boolean = false)
                                     (implicit g: GameInfo): Seq[Entity] = {
    val visited: mutable.Set[(Int, Int)] = mutable.Set()
    val toVisit: mutable.Queue[(Int, Int)] = mutable.Queue()
    val cameFrom: mutable.Map[(Int, Int), (Int, Int)] = mutable.Map()
    val res: mutable.Buffer[Entity] = mutable.Buffer()
    toVisit += ((x, y))
    visited += ((x, y))

    //cant start path from inaccessible cell
    val (canGo, cantGo) = rectNeighbours(x, y, size, g.mapSize, g.mapSize).partition(to => potentiallyWalkable(to, avoidUnits, avoidBuildings = true))
    toVisit ++= canGo
    cameFrom ++= canGo.map(c => c -> (x, y))
    visited ++= canGo
    visited ++= cantGo

    cantGo.foreach(x => g.entitiesMap(x.x)(x.y).foreach(e => if(filter(e)) res += e))


    while (toVisit.nonEmpty) {
      val (curx, cury) = toVisit.dequeue()
      val target = rectNeighbours(curx, cury, 1, g.mapSize, g.mapSize)
        .flatMap { case (x, y) => g.entitiesMap(x, y) }
        .filter(e => !res.contains(e) && filter(e))
        .take(count - res.size)
      res ++= target

      if(res.size >= count) return res.toSeq

      rectNeighbours(curx, cury, 1, g.mapSize, g.mapSize).foreach {
        neigh =>
          if (distance(neigh, (x, y)) <= maxDistance && potentiallyWalkable(neigh, avoidUnits, avoidBuildings = true) && !visited.contains(neigh)) {
            cameFrom(neigh) = (curx, cury)
            visited += neigh
            toVisit += neigh
          }
      }

    }
    return res.toSeq

  }

}

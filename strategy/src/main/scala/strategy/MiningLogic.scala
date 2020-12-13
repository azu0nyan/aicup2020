package strategy

import model.EntityType._
import model._
import strategy.BattleLogic.RegionInfo

import scala.util.Random

object MiningLogic extends StrategyPart {

//  val shortMineDistance = 5

//  val mediumMineDistance = 15

  val maxDangerToStay = 10

  /* def closestMine(point: (Int, Int))(implicit g: GameInfo): Option[Entity] =
     g.minableResource.filter(e => g.region(e.position.toProd).danger9 <= 15)
       .filter(m => rectNeighboursV(m.position, 1, g.mapSize, g.mapSize).exists(ne => g.))
       .minByOption(r => r.position.distanceTo(point))
 */
  override def getActions(implicit g: GameInfo): ActionMap = {

    var res: Map[Int, EntityAction] = Map()
    //mine neighbours if possible
    g.nonReservedWorkers
      .map(w => (w, rectNeighboursV(w.position, 1, g.mapSize, g.mapSize)
        .find { case (x, y) => g.entitiesMap(x)(y).exists(_.entityType == RESOURCE) })
      )
      .foreach {
        case (worker, Some((x, y))) =>
          res += g.mine(worker, g.entitiesMap(x)(y).get)
        case _ =>
      }
    //go to near resource if possible
    for (i <- Seq(1,2,3,7)) {
      g.nonReservedWorkers.filter(w => i >= g.regionsSize || g.region(w.position).minable9 > 0)
        .foreach { worker =>
          Pathfinding.findClosestReachable(worker.position.x, worker.position.y, x => g.minableResource.contains(x), i, avoidUnits = true)
            .foreach {
              case (resource, Seq()) =>
                res += g.mine(worker, resource)
              case (resource, x) =>
//                val sp = g.shortestPath(worker.position.toProd, x.last, avoidUnits = true, avoidBuildings = true)
                val sp = None
                //                if(!sp.contains(x)) {
                //                  println("-----")
                //                  println(worker.position.toProd, x.last)
                //                  println(x)
                //                  println(sp)
                //                }
                sp match {
//                  case Some(newPath) =>
//                    g.paths += newPath
//                    res += g.move(worker, newPath.head)
//                    g.minableResource -= resource
                  case None =>
                    g.paths += x
                    res += g.move(worker, x.head)
                    g.minableResource -= resource
                }

            }
        }
    }
    //go to near if no danger
    /* g.nonReservedWorkers.filter(w => g.region(w.position).danger9 <= maxDangerToStay)
      .foreach { worker =>
        g.findClosestReachable(worker.position.x, worker.position.y, x =>
          g.minableResource.contains(x), mediumMineDistance, avoidUnits = true)
          .foreach {
            case (resource, Seq()) =>
              res += g.mine(worker, resource)
            case (resource, x) =>
              g.paths += x
              res += g.move(worker, x.head)
              g.minableResource -= resource
          }
      }*/

    val mineRegions: Seq[RegionInfo] = g.regions.flatten
      .filter(r => r.danger9 <= maxDangerToStay && r.resources.nonEmpty && r.minable9 * 2 >= 0)
      .sortBy(-_.minable9)
      .sortBy(r => distance((0, 0), r.id))

    if (mineRegions.nonEmpty) {
      g.nonReservedWorkers.foreach { w =>
//        val reg = g.region(w.position)

        val candidats = mineRegions.take(10)
        //      mineRegions.minByOption(r => distance(r.id, reg.id)).foreach{reg =>

        val reg = candidats(new Random(w.id).nextInt(1000) % (candidats.size))
        res += g.goToRegion(w, reg, true, true)

      }
    }


    res


  }
}

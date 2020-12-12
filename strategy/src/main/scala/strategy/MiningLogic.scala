package strategy

import model.EntityType._
import model._
import strategy.BattleLogic.RegionInfo

object MiningLogic extends StrategyPart {

  val shortMineDistance = 7

  val mediumMineDistance = 15

  val maxDangerToStay = 15

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
    for(i <- 1 to shortMineDistance) {
      g.nonReservedWorkers//.filter(w => i >= g.regionsSize || g.region(w.position).resources9 > 0)
        .foreach { worker =>
          g.findClosestReachable(worker.position.x, worker.position.y, x => g.minableResource.contains(x), i, avoidUnits = true)
            .foreach {
              case (resource, Seq()) =>
                res += g.mine(worker, resource)
              case (resource, x) =>
                g.paths += x
                res += g.move(worker, x.head)
                g.minableResource -= resource
            }
        }
    }
    //go to near if no danger
    g.nonReservedWorkers.filter(w => g.region(w.position).danger9 <= maxDangerToStay)
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
      }

    val mineRegions: Seq[RegionInfo] = g.regions.flatten
      .filter(r => r.danger9 <= maxDangerToStay && r.resources.size >= 0 && r.resources.size >= r.my(BUILDER_UNIT).size).sortBy(-_.resources.size)

    g.nonReservedWorkers.foreach { w =>
      val reg = g.region(w.position)
      mineRegions.sortBy(r => distance(r.id, reg.id))
      mineRegions.headOption.foreach{reg =>

        res += g.goToRegion(w, reg, true, true)

      }

    }

    res


  }
}

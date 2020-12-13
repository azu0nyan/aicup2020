package strategy

import model._
import model.EntityType
import model.EntityType.{BUILDER_BASE, RANGED_BASE, RANGED_UNIT}

case class MacroState(g:GameInfo) {

  val ourTerritory = 50

  val playerPowers: Map[Player, Int] =
    g.pw.players.map(p => (p, g.entitiesByPlayer(p).map(_.entityType.attack.map(_.damage).getOrElse(0)).sum)) toMap

  val myPower: Int = playerPowers(g.me)

  val bases:Map[Player, (Int, Int)] = g.pw.players.map(p => (p, g.entitiesByPlayerIdAndType(p.id,RANGED_BASE ))).filter(_._2.nonEmpty)
    .map{case (p, b) => (p, rectNeighboursV(b.head.position, b.head.entityType.size, g.mapSize, g.mapSize)
      .filter(c => g.potentiallyWalkable(c, false, false)).minBy(c => distance(c, (10, 10)))    )
    }.toMap

  val pathsToBases:Map[Player, Seq[(Int, Int)]] = bases.map{case (p, b) =>
    (p, g.shortestPath((10, 10), (b), false, false))
  }.toMap.filter(_._2.nonEmpty).map{case (pl, p) => (pl, p.get)}

  pathsToBases.values.foreach(p => g.paths += p)
  val noPathToEnemy: Boolean = pathsToBases.isEmpty || pathsToBases.keys.toSet == Set(g.me)

  val ourRegions = g.regions.flatten.filter(r => r.center.x < ourTerritory && r.center.y < ourTerritory)

  val ourPowerAtBase = ourRegions.map(_.power).sum
  val enemyPowerAtOurBase = ourRegions.map(_.danger).sum

}

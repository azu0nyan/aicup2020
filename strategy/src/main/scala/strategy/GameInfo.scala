package strategy

import helpers.ArrayGrid
import model.EntityType._
import model.{AttackProperties, Entity, EntityProperties, EntityType, Player, PlayerView, Vec2Int}

object GameInfo {


  var entityPrice: Map[EntityType, Int] = Map()
  var entityProperties: Map[EntityType, EntityProperties] = Map()

  def firstRead(pw: PlayerView): Unit = {
    entityProperties = pw.entityProperties
    entityPrice = pw.entityProperties.map { case (entityType, properties) => (entityType, properties.initialCost) }
  }
}


class GameInfo(val pw: PlayerView) {

  implicit val g:GameInfo = this

  val resources: Seq[Entity] = pw.entities.filter(_.entityType == RESOURCE)

  val allTypes: Seq[EntityType] = Seq(WALL,
    HOUSE,
    BUILDER_BASE,
    BUILDER_UNIT,
    MELEE_BASE,
    MELEE_UNIT,
    RANGED_BASE,
    RANGED_UNIT,
    RESOURCE,
    TURRET)


  val me: Player = pw.players.find(_.id == pw.myId).getOrElse(Player(0, 0, 0))
  val enemies: Seq[Player] = pw.players.filter(_.id != pw.myId)
  val baseArea: Int = 30


  val entitiesByPlayerMap: Map[Player, Seq[Entity]] =
    pw.players.map(pl => (pl, pw.entities.filter(x => x.playerId.nonEmpty && x.playerId.get == pl.id))).toMap

  def entitiesByPlayer(p: Player): Seq[Entity] = entitiesByPlayerMap.getOrElse(p, Seq())

  val owned: Map[(Player, EntityType), Seq[Entity]] =
    (for (p <- pw.players; e <- allTypes) yield ((p, e), entitiesByPlayer(p).filter(_.entityType == e))).toMap


  val myEntities: Seq[Entity] = entitiesByPlayer(me)

  def unitCost(e: EntityType): Int = e.initialCost + my(e).size

  def myProductionBuildings: Seq[Entity] = myEntities.filter(_.entityType match {
    case BUILDER_BASE | MELEE_BASE | RANGED_BASE => true
    case _ => false
  })


  private val pmy: Map[EntityType, Seq[Entity]] = entitiesByPlayer(me).groupBy(_.entityType)
  def my(e: EntityType): Seq[Entity] = pmy.getOrElse(e, Seq())

  val entitiesMap: ArrayGrid[Option[Entity]] =
    new ArrayGrid[Option[Entity]](Array.fill[Option[Entity]](pw.mapSize * pw.mapSize)(None), (pw.mapSize, pw.mapSize))
  pw.entities.foreach(e => {
    for (i <- e.position.x until (e.position.x + e.entityType.size); j <- e.position.y until (e.position.y + e.entityType.size)) {
      entitiesMap.setValue((i, j), Some(e))
    }
  })


  def cellToEntity(pos: (Int, Int)): Option[Entity] = entitiesMap.valueAtUnsafe(pos)

  val cantBuildArea: ArrayGrid[Boolean] =
    new ArrayGrid[Boolean](Array.fill[Boolean](pw.mapSize * pw.mapSize)(false), (pw.mapSize, pw.mapSize))

  pw.entities.foreach(e => {
    if (isProductionBuilding(e.entityType)) {
      for (i <- (e.position.x - 1) until (e.position.x + e.entityType.size + 1); j <- (e.position.y - 1) until (e.position.y + e.entityType.size + 1)) {
        cantBuildArea.setValue((i, j), true)
      }
    } else {
      for (i <- e.position.x until (e.position.x + e.entityType.size); j <- e.position.y until (e.position.y + e.entityType.size)) {
        cantBuildArea.setValue((i, j), true)
      }
    }
  })

  val entitiesByPlayerId: Map[Int, Seq[Entity]] =
    (for (player <- pw.players) yield (player.id, entitiesByPlayer(player))) toMap

  val entitiesByPlayerIdAndType: Map[(Int, EntityType), Seq[Entity]] =
    (for (
      p <- pw.players.map(_.id);
      t <- allTypes
    ) yield ((p, t), entitiesByPlayerId(p).filter(_.entityType == t))).toMap


  val myWorkers: Seq[Entity] = entitiesByPlayerIdAndType(me.id, BUILDER_UNIT)
  val myRangedUnits: Seq[Entity] = entitiesByPlayerIdAndType(me.id, RANGED_UNIT)
  val myMeleeUnits: Seq[Entity] = entitiesByPlayerIdAndType(me.id, MELEE_UNIT)
  val myBuildings: Seq[Entity] = myEntities.filter(x => isBuilding(x.entityType))
  val armyTotal: Int = myRangedUnits.size + myMeleeUnits.size

  val populationUse: Int = entitiesByPlayer(me).map(_.entityType.populationUse).sum

  val populationMax: Int = entitiesByPlayer(me).map(_.entityType.populationProvide).sum

  val enemyEntities: Seq[Entity] = enemies.flatMap(entitiesByPlayer)

  //maps

  val dangerMap: Array[Array[Int]] = Array.ofDim(pw.mapSize, pw.mapSize)
  val powerMap: Array[Array[Int]] = Array.ofDim(pw.mapSize, pw.mapSize)

  myEntities.map(e => (e.position, e.entityType.attack)).foreach {
    case (Vec2Int(x, y), Some(AttackProperties(attackRange, damage, collectResource))) =>
      cellsInRange(x, y, attackRange, pw.mapSize).foreach { case (xx, yy) => powerMap(xx)(yy) += damage            }
    case _ =>
  }

  enemyEntities.map(e => (e.position, e.entityType.attack)).foreach {
    case (Vec2Int(x, y), Some(AttackProperties(attackRange, damage, collectResource))) =>
      cellsInRange(x, y, attackRange, pw.mapSize).foreach { case (xx, yy) => powerMap(xx)(yy) += damage }
    case _ =>
  }



  ///VARIABLES
  var myResources: Int = me.resource
  var populationFree: Int = populationMax - populationUse

  var minableResource:Set[Entity] = resources
    .filter(r => rectNeighbours(r.position.x, r.position.y, 1, pw.mapSize, pw.mapSize)
      .exists{ case (x, y) => entitiesMap(x, y).isEmpty ||
        entitiesMap(x, y).get.playerId.contains(me.id) && entitiesMap(x, y).get.entityType == BUILDER_UNIT }).toSet


  var reservedWorkers: Seq[Entity] = Seq()

  val nonActiveBuildings: Seq[Entity] = myBuildings.filter(!_.active)

  var nonReservedWorkers: Set[Entity] = myWorkers.toSet &~ reservedWorkers.toSet
}

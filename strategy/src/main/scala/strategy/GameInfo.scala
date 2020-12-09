package strategy

import helpers.ArrayGrid
import model.EntityType._
import model.{AttackProperties, Entity, EntityProperties, EntityType, Player, PlayerView, Vec2Int}
import strategy.BattleLogic.RegionInfo

import scala.annotation.tailrec
import scala.collection.mutable

object GameInfo {


  var entityPrice: Map[EntityType, Int] = Map()
  var entityProperties: Map[EntityType, EntityProperties] = Map()

  var myId = 0
  var firstReadHappened: Boolean = false

  def firstRead(pw: PlayerView): Unit = {
    myId = pw.myId
    entityProperties = pw.entityProperties
    entityPrice = pw.entityProperties.map { case (entityType, properties) => (entityType, properties.initialCost) }
  }
}


class GameInfo(val pw: PlayerView) {


  val mapSize: Int = pw.mapSize


  implicit val g: GameInfo = this

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


  val me: Player = pw.players.find(_.id == pw.myId).get
  val enemies: Seq[Player] = pw.players.filter(_.id != pw.myId)
  val baseArea: Int = 30


  val entitiesByPlayerMap: Map[Player, Seq[Entity]] =
    pw.players.map(pl => (pl, pw.entities.filter(x => x.playerId.nonEmpty && x.playerId.get == pl.id))).toMap

  def entitiesByPlayer(p: Player): Seq[Entity] = entitiesByPlayerMap.getOrElse(p, Seq())

  val owned: Map[(Player, EntityType), Seq[Entity]] =
    (for (p <- pw.players; e <- allTypes) yield ((p, e), entitiesByPlayer(p).filter(_.entityType == e))).toMap


  val myEntities: Seq[Entity] = entitiesByPlayer(me)

  val playerPowers: Map[Player, Int] =
    pw.players.map(p => (p, entitiesByPlayer(p).map(_.entityType.attack.map(_.damage).getOrElse(0)).sum)) toMap

  val myPower: Int = playerPowers(me)

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

  val populationMax: Int = entitiesByPlayer(me).filter(_.active).map(e => e.entityType.populationProvide).sum

  val populationMaxWithNonactive: Int = entitiesByPlayer(me).map(_.entityType.populationProvide).sum

  val enemyEntities: Seq[Entity] = enemies.flatMap(entitiesByPlayer)

  //maps

  val dangerMap: Array[Array[Int]] = Array.ofDim(pw.mapSize, pw.mapSize)
  val powerMap: Array[Array[Int]] = Array.ofDim(pw.mapSize, pw.mapSize)

  myEntities.map(e => (e.position, e.entityType.attack)).foreach {
    case (Vec2Int(x, y), Some(AttackProperties(attackRange, damage, collectResource))) =>
      cellsInRange(x, y, attackRange, pw.mapSize).foreach { case (xx, yy) => powerMap(xx)(yy) += damage }
    case _ =>
  }

  enemyEntities.map(e => (e.position, e.entityType.attack)).foreach {
    case (Vec2Int(x, y), Some(AttackProperties(attackRange, damage, collectResource))) =>
      cellsInRange(x, y, attackRange, pw.mapSize).foreach { case (xx, yy) => dangerMap(xx)(yy) += damage }
    case _ =>
  }

  val regionsSize: Int = 5
  val regionInSide: Int = g.pw.mapSize / regionsSize
  val regions: Array[Array[RegionInfo]] = Array.tabulate(g.regionInSide, g.regionInSide)((x, y) => new RegionInfo((x, y), (x * g.regionsSize, y * g.regionsSize)))
  val allRegions: Seq[RegionInfo] = regions.toSeq.flatten
  def region(pos: Vec2Int): RegionInfo = regions(pos.x / regionsSize)(pos.y / regionsSize)
  def region(x: Int, y: Int): RegionInfo = regions(x / regionsSize)(y / regionsSize)


  for (e <- myEntities) region(e.position).my_.updateWith(e.entityType) {
    case Some(seq) => Some(e +: seq)
    case None => Some(Seq(e))
  }
  for (e <- enemyEntities) region(e.position).enemy_.updateWith(e.entityType) {
    case Some(seq) => Some(e +: seq)
    case None => Some(Seq(e))
  }
  for (e <- resources) region(e.position).resources += e.health

  def isWalkable(xy:(Int, Int)):Boolean = entitiesMap(xy).isEmpty && !reservedForMovementCells.contains(xy)

  def findClosestReachable(x:Int, y:Int, filter: Entity => Boolean, maxDistance:Int) :Option[(Entity, Seq[(Int, Int)])] = {
    val visited:mutable.Set[(Int, Int)] = mutable.Set()
    val toVisit:mutable.Queue[(Int, Int)] = mutable.Queue()
    val cameFrom:mutable.Map[(Int, Int), (Int, Int)] = mutable.Map()
    toVisit += ((x, y))
    visited += ((x, y))

    /**without first node**/
    def  reconstructPath(to:(Int,Int)) :Seq[(Int, Int)] = cameFrom.get (to) match {
      case None => Seq()
      case Some(from) => to +: reconstructPath(from)
    }

    var found = false
    while (toVisit.nonEmpty){
      val (curx, cury) = toVisit.dequeue()
      val curE = entitiesMap(curx, cury)
      if(curE.nonEmpty && filter(curE.get)){
        return Some((curE.get, reconstructPath((curx, cury)).reverse))
      }
      rectNeighbours(curx, cury, 1, g.mapSize).foreach{
         xy => if(distance(xy, (x, y))<= maxDistance && isWalkable(xy) && !visited.contains(xy)){
           cameFrom(xy) = (x, y)
           visited += xy
           toVisit += xy
         }
      }
    }
    return None

  }

  //  def myUnitsInArea(cx:Int, cy:Int, size:Int, types:Seq[EntityType]):Seq[Entity] = {
  //    val regions:Set[(Int, Int)] =
  //      Set(
  //        ((cx /regionsSize, cy / regionsSize),
  //        (cx /regionsSize, cy / regionsSize),
  //        (cx /regionsSize, cy / regionsSize),
  //      )
  //  }
  //
  //

  ///VARIABLES
  var myMinerals: Int = me.resource
  var populationFree: Int = populationMax - populationUse

  var minableResource: Set[Entity] = resources
    .filter(r => rectNeighbours(r.position.x, r.position.y, 1, pw.mapSize, pw.mapSize)
      .exists { case (x, y) => entitiesMap(x, y).isEmpty ||
        entitiesMap(x, y).get.playerId.contains(me.id) && entitiesMap(x, y).get.entityType == BUILDER_UNIT
      }).toSet


  var reservedEnemy: Set[Entity] = Set()

  val damageToEnemy_ : mutable.Map[Entity, Int] = mutable.Map()
  def addDamageTo(enemy: Entity, damage: Int):Unit = damageToEnemy_.updateWith(enemy) {
    case Some(value) => Some(value + damage)
    case None => Some(damage)
  }
  def getDamageTo(enemy: Entity):Int = damageToEnemy_.getOrElse(enemy, 0)

  var reservedUnits: Set[Entity] = Set()

  def reservedWorkers: Set[Entity] = reservedUnits.filter(_.entityType == BUILDER_UNIT)

  var reservedBuildings: Set[Entity] = Set()

  val nonActiveBuildings: Seq[Entity] = myBuildings.filter(!_.active)

  val needRepairBUildings: Seq[Entity] = myBuildings.filter(x => !x.active | x.health != x.entityType.maxHealth)

  def nonReservedWorkers: Set[Entity] = myWorkers.toSet &~ reservedWorkers.toSet

  var reservedForMovementCells: Set[(Int, Int)] = Set()


  def probablyWalcable(x:Int, y:Int): Boolean = !reservedForMovementCells.contains((x, y)) &&
    (entitiesMap(x, y).isEmpty || isUnit(entitiesMap(x, y).get.entityType))

  def freeCell(x:Int, y:Int): Boolean = !reservedForMovementCells.contains((x, y)) && entitiesMap(x, y).isEmpty


}

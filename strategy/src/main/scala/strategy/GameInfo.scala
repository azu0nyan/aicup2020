package strategy

import com.sun.tools.javac.api.JavacTaskPool.Worker
import helpers.{ArrayGrid, BinHeap}
import model.EntityType._
import model.{AttackAction, AttackProperties, BuildAction, Entity, EntityAction, EntityProperties, EntityType, MoveAction, Player, PlayerView, RepairAction, Vec2Int}
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


class GameInfo(val pw: PlayerView, firstTick:Boolean) {

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



  def unitCost(e: EntityType): Int = e.initialCost + my(e).size

  def myProductionBuildings: Seq[Entity] = myEntities.filter(_.entityType match {
    case BUILDER_BASE | MELEE_BASE | RANGED_BASE => true
    case _ => false
  })


  private val pmy: Map[EntityType, Seq[Entity]] = entitiesByPlayer(me).groupBy(_.entityType)
  def my(e: EntityType): Seq[Entity] = pmy.getOrElse(e, Seq())

  val entitiesMap: Array[Array[Option[Entity]]] = Array.fill[Option[Entity]](mapSize, mapSize)(None)
  pw.entities.foreach(e => {
    for (i <- e.position.x until (e.position.x + e.entityType.size); j <- e.position.y until (e.position.y + e.entityType.size)) {
      entitiesMap(i)(j) = Some(e)
    }
  })
  def entitiesMap(tuple: (Int, Int)): Option[Entity] = entitiesMap(tuple._1)(tuple._2)


  def cellToEntity(pos: (Int, Int)): Option[Entity] = entitiesMap(pos)

  val cantBuildArea: Array[Array[Boolean]] = Array.fill[Boolean](mapSize, mapSize)(false)

  pw.entities.foreach(e => {
    val minx = math.max(0, e.position.x - 1)
    val maxx = math.min(e.position.x + e.entityType.size, mapSize -1)
    val miny = math.max(0, e.position.y - 1)
    val maxy = math.min(e.position.y + e.entityType.size, mapSize -1)
    if (isProductionBuilding(e.entityType)) {
      val minx = math.max(0, e.position.x - 1)
      val maxx = math.min(e.position.x + e.entityType.size, mapSize -1)
      val miny = math.max(0, e.position.y - 1)
      val maxy = math.min(e.position.y + e.entityType.size, mapSize -1)
      for (i <- minx to maxx; j <- miny to maxy) {
        cantBuildArea(i)(j) = true
      }
    } else {
      val minx = math.max(0, e.position.x )
      val maxx = math.min(e.position.x + e.entityType.size -1, mapSize -1)
      val miny = math.max(0, e.position.y )
      val maxy = math.min(e.position.y + e.entityType.size -1 , mapSize -1)
      for (i <- minx to maxx; j <- miny to maxy) {
        cantBuildArea(i)(j) = true
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
  if(!firstTick) {
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
  }

  val regionsSize: Int = 5
  val regionInSide: Int = g.pw.mapSize / regionsSize
  val regions: Array[Array[RegionInfo]] = Array.tabulate(g.regionInSide, g.regionInSide)((x, y) => new RegionInfo((x, y), (x * g.regionsSize, y * g.regionsSize)))
  val allRegions: Seq[RegionInfo] = regions.toSeq.flatten
  def region(pos: Vec2Int): RegionInfo = regions(pos.x / regionsSize)(pos.y / regionsSize)
  def region(x: Int, y: Int): RegionInfo = regions(x / regionsSize)(y / regionsSize)
  def region(xy: (Int, Int)): RegionInfo = regions(xy.x / regionsSize)(xy.y / regionsSize)

  if(!firstTick) {
    for (e <- myEntities) region(e.position).my_.updateWith(e.entityType) {
      case Some(seq) => Some(e +: seq)
      case None => Some(Seq(e))
    }
    for (e <- enemyEntities) region(e.position).enemy_.updateWith(e.entityType) {
      case Some(seq) => Some(e +: seq)
      case None => Some(Seq(e))
    }
    for (e <- resources) region(e.position).resources += e
  }





  ///VARIABLES
  var myMinerals: Int = me.resource
  var populationFree: Int = populationMax - populationUse

  var minableResource: Set[Entity] = resources
    .filter(r => rectNeighbours(r.position.x, r.position.y, 1, pw.mapSize, pw.mapSize)
      .exists { case (x, y) => entitiesMap(x, y).isEmpty ||
        entitiesMap(x, y).get.playerId.contains(me.id) && entitiesMap(x, y).get.entityType == BUILDER_UNIT
      }).toSet

  for (e <- minableResource) region(e.position).minable += 1

  var reservedEnemy: Set[Entity] = Set()

  val damageToEnemy_ : mutable.Map[Entity, Int] = mutable.Map()
  def addDamageTo(enemy: Entity, damage: Int): Unit = damageToEnemy_.updateWith(enemy) {
    case Some(value) => Some(value + damage)
    case None => Some(damage)
  }
  def getDamageTo(enemy: Entity): Int = damageToEnemy_.getOrElse(enemy, 0)

  var reservedUnits: Set[Entity] = Set()

  def reservedWorkers: Set[Entity] = reservedUnits.filter(_.entityType == BUILDER_UNIT)

  var reservedBuildings: Set[Entity] = Set()

  val nonActiveBuildings: Seq[Entity] = myBuildings.filter(!_.active)

  val needRepairBUildings: Seq[Entity] = myBuildings.filter(x => !x.active | x.health != x.entityType.maxHealth)

  def nonReservedWorkers: Set[Entity] = myWorkers.toSet &~ reservedWorkers.toSet

  var reservedForMovementCells: Set[(Int, Int)] = Set()
  //  var movingEntities:mutable.Map[Enti]

  var nextTurnPosition: Map[Entity, (Int, Int)] = Map()

  def canMoveToNextTurn(from: (Int, Int), to: (Int, Int)): Boolean = {
    !reservedForMovementCells.contains(to) && // cell not reserved
      distance(from, to) <= 1 && {
      entitiesMap(to).isEmpty || { //destination cell empty
        val atDestination = entitiesMap(to).get //entity at destination moving to other cell
        nextTurnPosition.contains(atDestination) && nextTurnPosition(atDestination) != from //not our cell preventing collision
      }
    }
  }

  //  def probablyWalkable(x:Int, y:Int): Boolean = !reservedForMovementCells.contains((x, y)) &&
  //    (entitiesMap(x, y).isEmpty || isUnit(entitiesMap(x, y).get.entityType))

  //  def freeCellNextTurn(x: Int, y: Int): Boolean = !reservedForMovementCells.contains((x, y)) && {
  //    entitiesMap(x, y).isEmpty ||
  //      movingEntities.contains(entitiesMap(x, y).get)
  //  }

  def emptyNextTurn(x: Int, y: Int): Boolean = !reservedForMovementCells.contains((x, y)) && {
    g.entitiesMap(x)(y).isEmpty //|| !nextTurnPosition.get(g.entitiesMap(x)(y).get).contains((x, y))
  }

  def attack(who: Entity, target: Entity)(implicit g: GameInfo): (Int, EntityAction) = {
    reservedUnits += who
    reservedForMovementCells += who.position.toProd
    nextTurnPosition += who -> who.position.toProd
    addDamageTo(target, who.damage)
    (who.id, EntityAction(None, None, Some(AttackAction(Some(target.id), None)), None))
  }

  def move(who: Entity, where: Vec2Int)(implicit g: GameInfo): (Int, EntityAction) = {
    reservedUnits += who
    nextTurnPosition += who -> where.toProd
    reservedForMovementCells += where.toProd
    (who.id, EntityAction(Some(MoveAction(where, false, false)), None, None, None))
  }

  def repair(worker: Entity, target: Entity): (Int, EntityAction) = {
    reservedUnits += worker
    reservedForMovementCells += worker.position.toProd
    nextTurnPosition += worker -> worker.position.toProd
    (worker.id, EntityAction(None, None, None, Some(RepairAction(target.id))))
  }

  def mine(worker: Entity, resource: Entity): (Int, EntityAction) = {
    region(resource.position).minable -= 1
    g.reservedUnits += worker
    g.minableResource -= resource
    reservedForMovementCells += worker.position.toProd
    nextTurnPosition += worker -> worker.position.toProd
    (worker.id, EntityAction(None, None, Some(AttackAction(Some(resource.id), None)), None))
  }

  def spawn(building: Entity, unitType: EntityType, producePosition: Vec2Int) = {
    println(s"Producing $unitType at $producePosition")
    g.populationFree -= unitType.populationUse //Update resource status
    g.myMinerals -= g.unitCost(unitType)
    g.reservedBuildings += building
    (building.id,
      EntityAction(
        None,
        Some(BuildAction(unitType, producePosition)),
        None,
        None
      )
    )
  }

  def build(pos: Vec2Int, builder: Entity, building: EntityType): (Int, EntityAction) = {
    g.reservedUnits += builder
    g.myMinerals -= building.buildScore
    rectArea(pos.x, pos.y, building.size, building.size, g.mapSize, g.mapSize).foreach(c => g.reservedForMovementCells += c)
    (builder.id, EntityAction(None, Some(BuildAction(building, (pos.x, pos.y))), None, None))
  }


  def goToRegion(unit: Entity, reg: RegionInfo, closest: Boolean, break: Boolean): (Int, EntityAction) = {
    g.reservedUnits += unit
    //todo
    (unit.id, EntityAction(Some(MoveAction(reg.center, closest, break)), None, None, None))
  }

  var paths: Set[Seq[(Int, Int)]] = Set()


  //// END VARS

  val macroState:MacroState =  if(Storage.firstRead){
    Storage.state = MacroState()(this)
    Storage.firstRead = false
    Storage.state
  }else if(pw.currentTick % Storage.readEvery == 0){
    Storage.state = MacroState()(this)
    Storage.firstRead = false
    Storage.state
  }else {
    Storage.state
  }

}

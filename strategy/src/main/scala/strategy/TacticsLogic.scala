package strategy

import model.EntityType._
import model._

import scala.collection.mutable

object TacticsLogic extends StrategyPart {

  def potentialTargets(who: Entity, types: Seq[EntityType])(implicit g: GameInfo): Seq[Entity] = {
    g.region(who.position).enemyN9(types).filter(target => canAttack(who, target))
  }

  def canAttack(who: Entity, target: Entity): Boolean = who.entityType.attack match {
    case Some(AttackProperties(r, _, _)) => target.position.distanceTo(who.position) <= r
    case None => false
  }

  def unitsThatCanAttack(enemy: Entity)(implicit g: GameInfo): Seq[Entity] =
    g.region(enemy.position).myN9(Seq(RANGED_UNIT, MELEE_UNIT, TURRET))
      .filter(u => !g.reservedUnits.contains(u) && canAttack(u, enemy))

  def findOneshotUnits(types: Seq[EntityType])(implicit g: GameInfo): Seq[Entity] =
    g.enemyEntities.filter(x => types.contains(x.entityType) && g.powerMap(x.position.x)(x.position.y) >= x.health)

  def findLethal(enemy: Entity)(implicit g: GameInfo): Option[Seq[Entity]] = {
    val units = unitsThatCanAttack(enemy)
    val totalDmg = units.map(_.damage).sum
    Option.when(totalDmg >= enemy.health)(
      units.sortBy(e => e.entityType match {
        case MELEE_UNIT => 1
        case RANGED_UNIT => 2
        case TURRET => 3
        case BUILDER_UNIT => 4
        case _ => 5
      }).foldLeft((Seq[Entity](), 0)) {
        case ((res, sum), cur) if sum < enemy.health => (res :+ cur, sum + cur.damage)
        case (x, _) => x
      }
    ).map(_._1)
  }

  def attack(who: Entity, target: Entity)(implicit g: GameInfo): (Int, EntityAction) = {
    g.reservedUnits += who
    g.reservedForMovementCells += who.position.toProd
    g.addDamageTo(target, who.damage)
    (who.id, EntityAction(None, None, Some(AttackAction(Some(target.id), None)), None))
  }

  def move(who: Entity, where: Vec2Int)(implicit g: GameInfo): (Int, EntityAction) = {
    g.reservedUnits += who
    g.reservedForMovementCells += where.toProd
    (who.id, EntityAction(Some(MoveAction(where, false, false)), None, None, None))
  }

  def getActions(implicit g: GameInfo): Map[Int, EntityAction] = {
    var res: mutable.Map[Int, EntityAction] = mutable.Map()

    //find oneshoot
    val unitsToDie = findOneshotUnits(Seq(RANGED_UNIT, MELEE_UNIT)).sortBy(e => e.entityType match {
      case RANGED_UNIT => 1
      case MELEE_UNIT => 2
      case BUILDER_UNIT => 3
    })

    for (
      enemy <- unitsToDie;
      units <- findLethal(enemy);
      u <- units) {
      g.reservedEnemy += enemy
      res += attack(u, enemy)
    }


    def findTargetToAttack(who: Entity, types: Seq[EntityType]): Option[Entity] =
      potentialTargets(who, types).filter(t => !g.reservedEnemy.contains(t)).sortBy(x => x.health - g.getDamageTo(x)).headOption

    def rangerFight(r: Entity) = {
      findTargetToAttack(r, Seq(RANGED_UNIT))
        .orElse(findTargetToAttack(r, Seq(MELEE_UNIT)))
        .orElse(findTargetToAttack(r, Seq(BUILDER_UNIT)))
        .orElse(findTargetToAttack(r, Seq(TURRET))).map {
        e => attack(r, e)
      }
    }

    def meleeFight(r: Entity) = rangerFight(r)


    def gotToClosestEnemy(r: Entity, maxRange: Int, types: Seq[EntityType]) =
      g.findClosestReachable(r.position.x, r.position.y, x => x.isEnemy && types.contains(x.entityType), maxRange).map {
        case (entity, path) => move(entity, path.head)
      }

    def gotToClosestFriend(r: Entity, maxRange: Int, types: Seq[EntityType]) =
      g.findClosestReachable(r.position.x, r.position.y, x => x != r && !x.isEnemy && types.contains(x.entityType), maxRange)
        .filter(x => x._1.position.distanceTo(r.position) != 1).map { //we already close to closest
        case (entity, path) => move(entity, path.head)
      }

    def goToBestPower(r: Entity): Option[(Int, EntityAction)] =
      cellsInRangeV(r.position, 1, g.mapSize)
        .filterNot(x => g.reservedForMovementCells.contains(x)).maxByOption({ case (x, y) => g.powerMap(x)(y) })
        .filterNot(x => x == r.position.toProd)
        .map { case (x, y) => move(r, Vec2Int(x, y)) }

    def goToLeastDamageIn5(r: Entity): Option[(Int, EntityAction)] =
      cellsInRangeV(r.position, 1, g.mapSize)
        .filterNot(x => g.reservedForMovementCells.contains(x))
        .minByOption({ case (x, y) => damageIn(x, y, 1) })
        .filterNot(x => x == r.position.toProd)
        .map { case (x, y) => move(r, Vec2Int(x, y)) }

    def damageIn(x: Int, y: Int, size: Int): Int = cellsInRange(x, y, size, g.mapSize).map { case (x, y) => g.dangerMap(x)(y) }.sum

    def rangerAi(r: Entity) = {
      val possibleDamageTaken = g.dangerMap(r.position.x)(r.position.y)
      val powerAtPosition = g.powerMap(r.position.x)(r.position.y)
      if (possibleDamageTaken > 0) { //possible this turn damage
        val stayAtPosition = possibleDamageTaken <= powerAtPosition
        val weProbablyDead = possibleDamageTaken >= 3 * r.health
        if (stayAtPosition | weProbablyDead) {
          rangerFight(r).orElse {
            Option.when(weProbablyDead)(goToBestPower(r)).flatten
          }.map {case (i, e) => res += i -> e}
        } else {
          goToLeastDamageIn5(r).orElse(rangerFight(r)).orElse(goToBestPower(r)).map(res += _)
        }
      } else {
        val neighDamage = damageIn(r.position.x, r.position.y, 1)
        if (neighDamage >= 5) { // neighbour cell on fire
          val noDanger = powerAtPosition > neighDamage
          if (noDanger) rangerFight(r).orElse(goToBestPower(r)).map(res += _)
          else goToLeastDamageIn5(r).orElse(rangerFight(r)).orElse(goToBestPower(r)).map(res += _)
        } else rangerFight(r).map(res += _)
      }
    }

    def meleeAi(m: Entity) = {
      val possibleDamageTaken = g.dangerMap(m.position.x)(m.position.y)
      val powerAtPosition = g.powerMap(m.position.x)(m.position.y)
      val reg = g.region(m.position)
      if (possibleDamageTaken > 0) { //possible this turn damage
        val stayAtPosition = reg.power9 >= reg.danger9 * 1.1
        val weProbablyDead = possibleDamageTaken >= m.health
        if (stayAtPosition | weProbablyDead) {
          meleeFight(m).orElse {
            gotToClosestEnemy(m, 6, Seq(RANGED_UNIT, MELEE_UNIT, BUILDER_UNIT, TURRET))
          }.map(res += _)
        } else {
//          goToLeastDamageIn5(m).map(res += _)
          gotToClosestEnemy(m, 6, Seq(RANGED_UNIT, MELEE_UNIT, BUILDER_UNIT, TURRET)).map(res += _)
        }
      } else {
        val neighDamage = damageIn(m.position.x, m.position.y, 2)
        if (neighDamage >= 5) { // neighbour cell on fire
          gotToClosestEnemy(m, 5, Seq(RANGED_UNIT, MELEE_UNIT, TURRET))
            .orElse(gotToClosestEnemy(m, 5, Seq(BUILDER_UNIT)))
            .orElse(gotToClosestFriend(m, 5, Seq(MELEE_UNIT))).map(res += _)
        } else {

        }
      }
    }

    def turretAi(m:Entity) = {
      rangerFight(m)
    }

    for (r <- g.myRangedUnits.filter(r => !g.reservedUnits.contains(r))) {
      rangerAi(r)
    }

    for (t <- g.my(TURRET).filter(t => !g.reservedUnits.contains(t))) {
      turretAi(t)
    }

    for (m <- g.myMeleeUnits.filter(m => !g.reservedUnits.contains(m))) {
      meleeAi(m)
    }

    res.toMap
  }

}

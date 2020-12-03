package strategy

import model._
import model.EntityType._
import org.graalvm.compiler.lir.alloc.lsra.OptimizingLinearScanWalker

import scala.collection.mutable
import scala.util.Random

object BattleLogic extends StrategyPart {


  override def getActions(implicit g: GameInfo): ActionMap = {


    (g.my(RANGED_UNIT) ++ g.my(MELEE_UNIT)).map{ u =>
      (u.id, g.enemyEntities.minBy(e => u.position.distanceTo(e.position)) match {
          case e if u.position.distanceTo(e.position) <= u.entityType.attack.get.attackRange =>
            EntityAction(None, None, Some(AttackAction(Some(e.id), None)), None)
          case e =>
            EntityAction(Some(MoveAction(e.position, true, true)), None, Some(AttackAction(Some(e.id), None)), None)

        })

    }
//    g.my(RANGED_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap ++
//    g.my(MELEE_UNIT).map(x => (x.id, region(x.position.x, x.position.y).recommendedAction.getOrElse(macroMovement(x)))).toMap
  } toMap
}

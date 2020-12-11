package strategy

import model.EntityType._
import model._

object MinersAvoidDamageLogic extends StrategyPart {


  override def getActions(implicit g: GameInfo): ActionMap = {
    g.nonReservedWorkers.filter(
      b => cellsInRangeV(b.position, 2, g.mapSize).exists { case (x, y) => g.dangerMap(x)(y) > 0 }
    ).flatMap { builder =>
      val nbrs = rectNeighboursV(builder.position, 1, g.mapSize, g.mapSize)
        .filter { case (x, y) => g.canMoveToNextTurn(builder.position.toProd, (x, y))}
      val target: Option[(Int, Int)] = nbrs.find { case (x, y) =>
        g.dangerMap(x)(y) == 0 &&
          cellsInRange(x, y, 2, g.mapSize).forall { case (x, y) => g.dangerMap(x)(y) == 0 }
      }.orElse {
        ((builder.position.x, builder.position.y) +: nbrs).find { case (x, y) =>
          g.dangerMap(x)(y) == 0 &&
            cellsInRange(x, y, 1, g.mapSize).forall { case (x, y) => g.dangerMap(x)(y) == 0 }
        }
      }.orElse(
        ((builder.position.x, builder.position.y) +: nbrs).minByOption { case (x, y) =>
          cellsInRange(x, y, 1, g.mapSize).map { case (x, y) => g.dangerMap(x)(y) }.sum
        }
      )

      target.map { t =>
        g.reservedUnits += builder
        (builder.id, EntityAction(Some(MoveAction(t, true, false)), None, None, None))
      }
    }
  }.toMap
}

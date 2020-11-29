package strategy

trait StrategyPart {
  def getActions(implicit g: GameInfo): ActionMap
}

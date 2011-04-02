

sealed abstract class BotType
case class DotBot() extends BotType
case class RectBot() extends BotType


sealed abstract class Intention

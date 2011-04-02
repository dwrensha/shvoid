//package shvoid

sealed abstract class BotType
case class DotBot() extends BotType
case class RectBot() extends BotType

sealed abstract class TurnIntent
case object TurnLeft extends TurnIntent
case object TurnRight extends TurnIntent

sealed abstract class AccelIntent
case object Accel extends AccelIntent
case object Brake extends AccelIntent




object Types {


  class SyncMap[A,B] 
     extends scala.collection.mutable.HashMap[A,B] with 
             scala.collection.mutable.SynchronizedMap[A,B]

  type HashMap[A,B] = scala.collection.mutable.HashMap[A,B] 

  type BotID = Int


  type Intent = (Option[TurnIntent], Option[AccelIntent])

  
}

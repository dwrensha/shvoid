

sealed abstract class BotType
case class DotBot() extends BotType
case class RectBot() extends BotType

sealed abstract class Intention



object Types {



  class HashMap[A,B] 
     extends scala.collection.mutable.HashMap[A,B] with 
             scala.collection.mutable.SynchronizedMap[A,B]

  type BotID = Int
  
}

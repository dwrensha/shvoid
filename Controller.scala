//package shvoid

import scala.actors.Actor
import scala.actors.Actor._
import Types._

class Controller(intents: SyncMap[BotID, Intent] ) extends Actor {

//  case class BotInfo(next: Option[BotID] )

  //val bots : HashMap[BotID, ()] = new

//  var intents : SyncMap[BotID, Intent] = it
  
  

  def act() : Unit = {
    println("hello from controller")

    var receivemore : Boolean = true

    while(true) {

      // get everything from a single step
      receivemore = true
      while(receivemore){
        receive {
          case ('BotSpawn, id, spawn, v, gl, angle, omega ) =>
            println("a bot spawned.")
          case ('BotDone, id ) =>
            println("a bot reached its goal.")
          case 'StepDone => 
            receivemore = false
          case msg => 
            println("got message: " + msg)
            
        }
      }

      // update


      println(intents.size)



    }


  }


}

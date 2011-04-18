//package shvoid

import scala.actors.Actor
import scala.actors.Actor._

import org.jbox2d.common.Vec2

import Types._

class Controller(intents: SyncMap[BotID, Intent] ) extends Actor {

  case class BotInfo(pos : Vec2,
                     vel : Vec2,
                     next: BotID, // position of next car
                     obs : List[Vec2] //position of obstacles to avoid
                     )



  val bots : HashMap[BotID, BotInfo] = new HashMap[BotID,BotInfo]()

  val lanetails = Array(nobot,nobot,nobot, nobot)

//  var intents : SyncMap[BotID, Intent] = it
  
  

  def act() : Unit = {
    println("hello from controller")

    var receivemore : Boolean = true

    while(true) {

      // get everything from a single step
      receivemore = true
      while(receivemore){
        receive {
          case ('BotSpawn, 
                id: BotID, 
                p : Vec2, 
                v : Vec2, 
                angle: Float,
                omega: Float,
                lane : Int) =>
            println("a bot spawned.")
            bots.put(id, BotInfo(p,v, lanetails(lane), Nil))
            lanetails.update(lane, id)
          case ('BotUpdate, id: BotID, p: Vec2, v: Vec2) => 
            bots.get(id) match {
              case Some(BotInfo(p0,v0,n0,obs0)) =>
                bots.put(id,BotInfo(p,v,n0,obs0))
              case None => 
            }

          case ('BotDone, id ) =>
            println("a bot reached its goal.")
          case 'StepDone => 
            receivemore = false
          case msg => 
            println("got message: " + msg)
            
        }
      }

      // update


      //println(intents.size)



    }


  }


}

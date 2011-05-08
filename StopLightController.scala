//package shvoid

import scala.actors.Actor
import scala.actors.Actor._

import org.jbox2d.common.Vec2

import Types._

class StopLightController(intents: SyncMap[BotID, Intent], 
                 MAX_A : Float, 
                 MAX_B : Float,
                 EPS : Float) extends Actor {

  case class BotInfo(pos : Vec2,
                     vel : Vec2,
                     next: BotID, //  next car
                     obs : List[Vec2] //position of obstacles to avoid
                     )


  val  rand = new scala.util.Random(System.currentTimeMillis())

  val bots : HashMap[BotID, BotInfo] = new HashMap[BotID,BotInfo]()

  val intersection = new Vec2(0f,0f)

  var simulationTime = 0f

  
  

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
            bots.put(id, BotInfo(p,v, 0, List(intersection ) ))
          case ('BotUpdate, id: BotID, p: Vec2, v: Vec2) => 
            bots.get(id) match {
              case Some(BotInfo(p0,v0,n0,obs0)) =>
                bots.put(id,BotInfo(p,v,n0,obs0))
              case None => 
            }

          case ('BotDone, id: BotID ) =>
            println("a bot reached its goal.")
            bots.remove(id)
          case 'StepDone => 
            receivemore = false
          case ('Time, t: Float) => 
            simulationTime = t            
          case msg => 
            println("got message: " + msg)
            
        }
      }



      // Each car makes a decision about what to do.

      for((id,BotInfo(p,v,nxt,obs)) <- bots) {
        intents.put(id,(None,Some(Accel)))
      }




    }


  }


}

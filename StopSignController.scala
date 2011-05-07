//package shvoid

import scala.actors.Actor
import scala.actors.Actor._

import org.jbox2d.common.Vec2

import Types._

class StopSignController(intents: SyncMap[BotID, Intent], 
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

  val lanetails = Array(nobot,nobot,nobot, nobot)



  val intersection = new Vec2(0f,0f)
  var lockHolder : Option[BotID]= None

  
  var simulationTime = 0f



  val FOLLOW_DISTANCE = 4f;
  val INTERSECTION_DISTANCE = 6f;
  val RELEASE_LOCK_DISTANCE = 4f;

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
            bots.put(id, BotInfo(p,v, lanetails(lane), List(intersection ) ))
            lanetails.update(lane, id)
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
        if(lockHolder == Some(id) 
           && v.length() > 0.1f
           &&  Vec2.dot(p.sub(intersection),v) > 0f 
           && p.sub(intersection).length > RELEASE_LOCK_DISTANCE){
             lockHolder = None
           }

        bots.get(nxt) match {
          case Some(BotInfo(p1,v1,_,_)) 
           if 2f * MAX_B * (p.sub(p1).length  - FOLLOW_DISTANCE ) < 
               v.lengthSquared() - v1.lengthSquared() + 
               (MAX_A + MAX_B) * (MAX_A * EPS * EPS + 2f * EPS * v.length)
            => intents.put(id,(None,Some(Brake)))
          case _ =>
             obs match {
              case List(ob)
                if 
                  2f * MAX_B * (p.sub(ob).length - INTERSECTION_DISTANCE ) < 
                  v.lengthSquared() + 
                  (MAX_A + MAX_B) * (MAX_A * EPS * EPS + 2f * EPS * v.length)
                =>
                  lockHolder match {
                    case Some(_) => 
                      intents.put(id,(None,Some(Brake)))
                    case None =>
                      println("GRABBING LOCK. " + id)
                      lockHolder = Some(id)
                      bots.put(id, BotInfo(p,v,nxt,Nil))
                  }
                
              case _ => 
                val r = rand.nextDouble
                if (r < 0.99){
                  intents.put(id,(None,Some(Accel)))
                } else if (r < 0.999) {
                  intents.put(id,(None,None))
                } else {
                  intents.put(id,(None,Some(Brake)))
                }

            }            
        }
        }

      //println("lockholder = " + lockHolder)
      //println("mailbox size = " + mailboxSize)



    }


  }


}

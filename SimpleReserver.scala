//package shvoid

import scala.actors.Actor
import scala.actors.Actor._

import org.jbox2d.common.Vec2

import Types._


/*
 * Lane numbers and lock numbers.
  */

  /****2 ****
   ****  ****
   ****  ****
       23   3
   1   10
   ****  ****
   ****  ****
   **** 0****
  */


/*********
 *********  
  ******** 
  *
  *
  *
  *
  *
  *
  *
  *
  *
  *
  *
  *
  *
  *
  */ 


class SimpleReserverController(intents: SyncMap[BotID, Intent], 
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

  type Time = Float
  type Reservation = (Time,Time) // Start time and length

  val reservations : Array[List[Reservation]] = Array(Nil,Nil,Nil,Nil)


  val FOLLOW_DISTANCE = 4f;
  val INTERSECTION_DISTANCE = 6f;
  val RELEASE_LOCK_DISTANCE = 4f;

//  var intents : SyncMap[BotID, Intent] = it
  
  
  def square(x: Float): Float = {x * x}

  def canMakeReservation(rx1:Float,rx2:Float,rt1:Float,rt2:Float,x0:Float,v0:Float, t0:Float) : Boolean = {
    val l1 = rx1 - x0
    val l2 = rx2 - x0
    val dt1 = rt1 - t0
    val dt2 = rt2 - t0
    val stoptime = v0 / MAX_B


    // See if we can avoid getting there too soon.

    if(stoptime < rt1) { // we can stop before our reservation time
      if( 0.5f * v0 * stoptime > l1) { // we've passed the gate by the time we've stopped
        return false
      }
    } else { 
      if (-0.5f * MAX_B * dt1 * dt1 + v0 * dt1 > l1  ) { // we've passed the gate 
        return false
      }
    }

    // See if we even need to worry about the first gate

    if ( 0.5f * MAX_A * dt1 * dt1 + v0 * dt1 < l1  ) {  // we can't make it 
      // so we might as well floor it.
      if(0.5f * MAX_A * dt2 * dt2 + v0 * dt1 >= l2) {
        return true
      } else {
        return false
      }
      
    } 

    // ok, we can avoid getting there too soon. can we cross the second gate before it closes?
    val remainingtime = dt1 - stoptime
    val tbg = dt2-dt1 // time between gates

    if (0.5f * (v0 * stoptime + remainingtime * remainingtime * MAX_A )  < l1 ){ //If we stop, we're not there for the gate's opening
      val floorItTime = (dt1 * ( MAX_A + MAX_B) + scala.math.sqrt( square(dt1 * (MAX_A + MAX_B) ) - (MAX_A + MAX_B) *(MAX_A * dt1*dt1 + v0*dt1 - l1)    )   )   / (MAX_A + MAX_B)
      val openVel = v0 - MAX_B *  floorItTime +  MAX_A * (rt1 - floorItTime)

      val finishX = 0.5f * MAX_A * tbg * tbg + openVel * tbg + l1
      if (finishX > l2){
        return true
      } else {
        return false
      }
    } else { // strategy: stop, then accelerate at the last possible instant to get there in time for gate's opening.
      val remainingX = l1 - stoptime * v0
      val floorItTime = scala.math.sqrt(2f * remainingX / MAX_A)
      val openVel = MAX_A * (dt1 - floorItTime)
      
      val finishX = 0.5f * MAX_A * tbg * tbg + openVel * tbg + l1
      if (finishX > l2){
        return true
      } else {
        return false
      }

    }
    


  }


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
                if (r < 0.8){
                  intents.put(id,(None,Some(Accel)))
                } else if (r < 0.9) {
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

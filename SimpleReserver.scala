//package shvoid

import scala.actors.Actor
import scala.actors.Actor._

import org.jbox2d.common.Vec2

import Types._
import Lanes._


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

  type Time = Float
  type Reservation = (Time,Time) // Start time and end time

  case class BotInfo(pos : Vec2,
                     vel : Vec2,
                     next: BotID, //  next car
                     lane : Int, // lane
                     res : Option[Reservation]
                     )


  val rand = new scala.util.Random(System.currentTimeMillis())

  val bots : HashMap[BotID, BotInfo] = new HashMap[BotID,BotInfo]()

  val lanetails = Array(nobot,nobot,nobot, nobot)

  val intersection = new Vec2(0f,0f)
  
  var lockHolder : Option[BotID]= None

  var simulationTime = 0f



  val reservations : Array[List[Reservation]] = Array(Nil,Nil,Nil,Nil)


  val FOLLOW_DISTANCE = 4f;
  val INTERSECTION_DISTANCE = 6f;
  val RELEASE_LOCK_DISTANCE = 4f;

  val GATE1 = -4f
  val GATE2 = 4f

  

  def act() : Unit = {
    println("hello from SimpleReserver controller")

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
            bots.put(id, BotInfo(p,v, lanetails(lane), lane, None ))
            lanetails.update(lane, id)
          case ('BotUpdate, id: BotID, p: Vec2, v: Vec2) => 
            bots.get(id) match {
              case Some(BotInfo(p0,v0,n0,ln,r)) =>
                bots.put(id,BotInfo(p,v,n0,ln,r))
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

      for((id,BotInfo(wp,wv,nxt,ln,r)) <- bots) {
        println("-------------------")
        println("on car: " + id)
        println("current intent = " + intents.get(id))
        println("simulation time = " + simulationTime)
        val x = world2lane(wp,ln)
        val v = world2lane(wv, ln)
        println("x = " + x)
        println("v = " + v)
        if (x < GATE1) {
          var myres : Reservation = null
          r match {
            case None => // we need to make a reservation
              bots.get(nxt) match {
                case Some(BotInfo(_,_,_,_,Some((t1,t2)))) => // bot in front of us has a reservation.
                  makeReservation(x,v,t2,ln) match {
                    case Some(res) => 
                      bots.put(id,BotInfo(wp,wv,nxt,ln,Some(res)))
                      myres = res
                    case None => 
                  }
                case None => 
                  makeReservation(x,v, simulationTime ,ln) match { // XXX could put something here smarter than "simulationTime"
                    case Some(res) => bots.put(id,BotInfo(wp,wv,nxt,ln,Some(res)))
                      myres = res
                    case None => 
                  }
                case _ => 
                  
              }
            case Some((t1,t2)) =>
              myres = (t1,t2)
          }
          val (t1,t2) = myres

          
          if(canDoReservation(GATE1,GATE2, t1, t2,   
                                     v * EPS + x , 
                                     v , simulationTime + EPS)) {
            println("we can coast")
            intents.put(id,(None,None))
          } else if(canDoReservation(GATE1,GATE2, t1, t2,   
                                     0.5f * MAX_A * EPS * EPS + v * EPS + x , 
                                     v + MAX_A * EPS, simulationTime + EPS)) {
          //accelerate if possible
            println("we can accelerate")
            intents.put(id,(None,Some(Accel)))
          } else { // otherwise brake
            println("we should brake")
            intents.put(id,(None,Some(Brake)))
            //          intents.put(id,(None,Some(Accel)))
          }

        } else { // this car has passed the intersection
          

          intents.put(id,(None,Some(Accel)))
        }

      }

      //println("lockholder = " + lockHolder)
      //println("mailbox size = " + mailboxSize)


    }


  }




  val RES_EPS = 0.01f

  // all of this in lane coordinates
  def makeReservation(x0 : Float, v0 : Float, afterT : Float, laneNum : Int) : Option[Reservation] = {
    
    if(x0 > GATE1){ // we're already too late
      return None
    }

    val (ln1,ln2) = lane2tiles(laneNum)
    
//    var prevs1: List[Reservation] = Nil
//    var prevs2: List[Reservation] = Nil
    var nexts1: List[Reservation] = reservations(ln1)
    var nexts2: List[Reservation] = reservations(ln2)
    var newres : Option[Reservation] = None

    nexts1 = dropUntil(nexts1,afterT)
    nexts2 = dropUntil(nexts2,afterT)

    var currentT = afterT
    var nextPlausible = currentT
    while(newres.isEmpty){
      val t1 = currentT
      // calculate t2. be a bit conservative.
//      val myA = (0.8f * MAX_A)
      val dx = GATE1 - x0
      val dt =  (t1 - simulationTime)      
      val avgV = dx / dt
      val t2 = t1 +    2f * ( GATE2 - GATE1) / avgV
      if (0.5f * MAX_A * dt * dt + v0 * dt + x0 < GATE1){ 
        currentT += 0.2f
      } else {
        findConflict(nexts1,nexts2,(t1,t2)) match {
          case None =>  
            newres = Some((t1,t2))
          case Some(tconflict) => 
            currentT = tconflict + RES_EPS
        }
      }
    }
    val r = newres.get


    reservations.update(ln1,insertReservation(reservations(ln1), r))
    reservations.update(ln2,insertReservation(reservations(ln2), r))



    println("reservation made: " + r)
    return Some(r)

  }

  def insertReservation(lst: List[Reservation], r: Reservation) : List[Reservation] = {
    var prevs: List[Reservation] = Nil
    var nexts: List[Reservation] = lst
    
    val r1 = shiftUntil(prevs, nexts, r._1)

    r1._1.reverse ++ List(r) ++ r1._2
    
  }


  // return None if this works, otherwise, give a new starting time to try.
  def findConflict(lst1 : List[Reservation], lst2: List[Reservation], r: Reservation) : Option[Float] = {
    val (t1,t2) = r

    val lst11 = dropUntil(lst1, t1)
    val lst21 = dropUntil(lst2, t1)

    (lst11,lst21) match {
      case ((l1t1,l1t2)::_, (l2t1,l2t2)::_ ) => 
       if( l1t1 <= t2 ) {
          return Some(l1t2)
        } else if( l1t1 <= t2 ) {
          return Some(l1t2)
        } else {
          return None
        }
      case (Nil, (l2t1,l2t2)::_)  => // we know that l2t2 >= t1
       if((l2t1) <= t2) {
         return Some(l2t2)
       } else {return None}
      case ((l1t1,l1t2)::_,Nil) => // we know that l1t2 >= t1
       if((l1t1) <= t2) {
         return Some(l1t2)
       } else {return None}
      case _ => 
        return None
    }
    
    
  }

  // drop all reservations that cannot possibly
  // conflict with one that starts at time v
  def dropUntil(lst: List[(Reservation)], v: Float) : List[Reservation] = {
    var nexts = lst
    var prevs: List[Reservation] = Nil
    while((! nexts.isEmpty) && 
          nexts.head._2 < v){
      prevs = nexts.head :: prevs
      nexts = nexts.tail
    }
    nexts
  }


  def shiftUntil(racc: List[(Reservation)], lst: List[(Reservation)], v: Float) : ( List[Reservation], List[Reservation]  ) = {
    var nexts = lst
    var prevs: List[Reservation] = Nil
    while((! nexts.isEmpty) && 
          nexts.head._2 < v){
      prevs = nexts.head :: prevs
      nexts = nexts.tail
    }
    (prevs,nexts)
  }
  
  
  def square(x: Float): Float = {x * x}

  def canDoReservation(rx1:Float,rx2:Float,rt1:Float,rt2:Float,x0:Float,v0:Float, t0:Float) : Boolean = {
    val l1 = rx1 - x0
    val l2 = rx2 - x0
    val dt1 = rt1 - t0
    val dt2 = rt2 - t0
    val stoptime = v0 / MAX_B

    println("canDoReservation: " + rx1 + "," + rx2 + "," + rt1 + "," + rt2 + "," + x0 + "," + v0 + "," + t0)
    println("l1 = " + l1)
    println("l2 = " + l2)
    println("dt1 = " + dt1)
    println("dt2 = " + dt2)
    println("stoptime = " + stoptime)
    println("MAX_A = " + MAX_A)

    // See if we can avoid getting there too soon.

    if(stoptime < dt1) { // we can stop before (in time) the first gate opens
      if( 0.5f * v0 * stoptime > l1) { // we've passed the gate by the time we've stopped
        return false
      }
    } else { // we can't stop before the first gate opens
      if (-0.5f * MAX_B * dt1 * dt1 + v0 * dt1 > l1  ) { // we've passed the gate when it opens
        return false
      }
    }

    println("we can avoid getting there too soon")

    // See if we even need to worry about the first gate

    if ( 0.5f * MAX_A * dt1 * dt1 + v0 * dt1 < l1  ) {  // we can't make it 
      println("we might as well floor it.")

      // so we might as well floor it.
      if(0.5f * MAX_A * dt2 * dt2 + v0 * dt1 >= l2) {
        return true
      } else {
        println("we can't even make it then!")
        return false
      }
      
    } 

    println("we do have to worry about the first gate")

    // ok, we can avoid getting there too soon. can we cross the second gate before it closes?
    val remainingtime = dt1 - stoptime

    println("remainingtime = " + remainingtime)
    val tbg = dt2-dt1 // time between gates

    if (0.5f * (v0 * stoptime + remainingtime * remainingtime * MAX_A )  < l1 ){ //If we stop, we're not there for the gate's opening
      println("we could stop before the gate opens")
      // this is a time relative to t0
      val floorItTime = (dt1 * ( MAX_A + MAX_B) - scala.math.sqrt( square(dt1 * (MAX_A + MAX_B) ) - (MAX_A + MAX_B) *(MAX_A * dt1*dt1 + v0*dt1 - l1)    )   )   / (MAX_A + MAX_B)
      val openVel = v0 - MAX_B *  floorItTime +  MAX_A * (dt1 - floorItTime)

      val finishX = 0.5f * MAX_A * tbg * tbg + openVel * tbg + l1

      println("floor it time = " + floorItTime)
      println("openVel = " + openVel)
      println("finishX = " + finishX)
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




}

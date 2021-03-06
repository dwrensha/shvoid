//package shvoid


import java.util.ArrayList

import org.jbox2d.common.Color3f
import org.jbox2d.common.Vec2
import org.jbox2d.collision.AABB
import org.jbox2d.collision.PolygonDef
import org.jbox2d.dynamics.Body
import org.jbox2d.dynamics.BodyDef
import org.jbox2d.dynamics.DebugDraw
import org.jbox2d.dynamics.World

import processing.core._
import processing.core.PConstants._



import Types._
import PhysicsTypes._
import Lanes._


class Physics extends PApplet {

 

    val zerovec = new Vec2(0f,0f)

    /** Was the mouse down last frame? */
    var pmousePressed = false
    
    /** FPS that we want to achieve */
    val targetFPS = 60.0f
    
    var avgFPS = 0.0
    val numFrames = 100
    var frameNum = 0

    var numBotsDone = 0

    val spawnProbs :  Array[Double] = Array(0.01, 0.01, 0.01, 0.01)
    var spawnDelay : Int = 20

    var startTime : Long = System.currentTimeMillis()

    var simulationTime: Float = 0f

    val lock : Object = new Object()

    /** Drawing handler to use. */
    var dd: DebugDraw = null

    var world: World = null



    
    var livebots : HashMap[BotID, LiveBotInfo]  = 
      new HashMap[BotID,LiveBotInfo]()    
    var donebots : HashMap[BotID, DoneBotInfo]  = 
      new HashMap[BotID,DoneBotInfo]()    
//    var positions : HashMap[BotID, Vec2]  = new HashMap[BotID,Vec2]()    
    var intents : SyncMap[BotID, Intent] = new SyncMap[BotID, Intent]()

    private var nextBotID = 0;

//    val maxspeed = 10.0f;
//    val maxomega = 5.0f;
  val MAXFORCE = 15.0f;
  val MAXBRAKE = 15.0f;
  val MAXTORQUE = 5.0f;

  val BOTLONG = 1.5f;
  val BOTWIDE = 1.0f;
  val BOTDENSE = 5.0f;
  val BOTMASS = BOTLONG * BOTWIDE * BOTDENSE;

  // conservatively estimate the polling cycle as half the frame time.
  val EPS = 2f / ( targetFPS) 

  var controller: scala.actors.Actor = 
    new Controller(intents, 
                   MAXFORCE / BOTMASS, 
                   MAXBRAKE / BOTMASS,
                   EPS)

  println("args = " + args)

  def trackFPS() = {
    frameNum += 1
    if (frameNum == numFrames){
      val newTime = System.currentTimeMillis()
      avgFPS = numFrames * 1000 /  (newTime - startTime).asInstanceOf[Double];
//     avgFPS =  1.0 *  (newTime - startTime);
      frameNum = 0
      startTime = newTime
    }

  }


    def makeBot(p: Vec2, v: Vec2, gl: Goal, theta:Float , omega: Float) : Int = {
      val sd: PolygonDef = new PolygonDef()
      sd.setAsBox(0.5f * BOTLONG, 0.5f * BOTWIDE)
      sd.density = BOTDENSE
      sd.restitution = 0.0f
      sd.friction = 0.5f

      val bd: BodyDef = new BodyDef()
      bd.position.set(p)
      bd.linearDamping = 0.0f
      bd.angularDamping = 0.5f
      bd.angle = theta
      val body: Body = world.createBody(bd)
      body.createShape(sd)
      body.setMassFromShapes()
      body.setLinearVelocity(v)
      body.setAngularVelocity( omega)
      
      val bid = nextBotID
      livebots.put(bid, new LiveBotInfo(body, 
                                        gl,  
                                        System.currentTimeMillis()))
      intents.put(bid,(None,Some(Accel)))
       

      nextBotID+= 1

      return nextBotID - 1;

    }


    def makeObstacle(p: Vec2, sz: Float) = {
      val sd: PolygonDef = new PolygonDef()
      sd.setAsBox(sz, sz)
      sd.restitution = 0.0f
      sd.friction = 0.5f

      val bd: BodyDef = new BodyDef()
      bd.position.set(p)
      val body: Body = world.createBody(bd)
      body.createShape(sd)

    }


  // hmm... apparently this gets called twice?
    override def setup() {
        println("setting up")
    	size(800,600,P3D)
    	frameRate(targetFPS)
    	dd = new ProcessingDebugDraw(this)
        this.requestFocus()


        val worldAABB:AABB = new AABB()
        worldAABB.lowerBound = new Vec2(-200.0f, -200.0f)
        worldAABB.upperBound = new Vec2(200.0f, 200.0f)
        val gravity:Vec2 = new Vec2(0.0f, 0.0f)
        val doSleep = true
        world = new World(worldAABB, gravity, doSleep)
        world.setDebugDraw(dd)
    	
        dd.appendFlags(DebugDraw.e_shapeBit);
        dd.setCamera(0.0f,0.0f, 3.0f);

        // add some stuff to the world.

/*
        makeBot(new Vec2(0.0f,0.0f),
                new Vec2(0.0f, 0.0f), 
                (new Vec2(80.0f, 0.0f), 5.0f),
                0.0f, 0.0f)
*/


      makeObstacle(new Vec2(-102.0f,-102.0f), 98.0f)
      makeObstacle(new Vec2(-102.0f,102.0f), 98.0f)

      makeObstacle(new Vec2(102.0f,-102.0f), 98.0f)
      makeObstacle(new Vec2(102.0f,102.0f), 98.0f)


//      controller = new Controller(intents, MAXFORCE, MAXBRAKE)
      controller.start()
                                        
    }
    
    /**
     * This is the main looping function,
     * and is called targetFPS times per second.
     */
    override def draw() {
        trackFPS()
        perhapsCreateBots()

        var toRemove: List[BotID] = Nil
        println("TIMESTEP " + simulationTime + " " + numBotsDone)

        for((id,botinfo) <- livebots ){
          val intent = intents.getOrElse(id,null)
          val b = botinfo.body
          val gl@(glv, glr) = botinfo.goal
          val p = b.getPosition()
          if(glv.sub(p).length() < glr){ // reached goal!
            controller ! (('BotDone, id ))
            val st = botinfo.starttime
            numBotsDone += 1
            toRemove = id :: toRemove
            donebots.put(id, new DoneBotInfo(gl, 
                                             st,
                                             System.currentTimeMillis()))

          }else {
            val theta = b.getAngle()
            val v = b.getLinearVelocity()
            val vmag = v.length()
            val u = new Vec2(scala.math.cos(theta).asInstanceOf[Float], 
                             scala.math.sin(theta).asInstanceOf[Float])
//          val _ = b.setLinearVelocity(u.mul(vmag))
            val (t,a) = intent
            controller ! (('BotUpdate, id, p,v ))
            t match {
              case Some(TurnLeft) => 
                b.applyTorque(MAXTORQUE)
              case Some(TurnRight) => 
                b.applyTorque(- MAXTORQUE)
              case None => 

            }
            a match {
              case Some(Accel) => 
                b.applyForce(u.mul(MAXFORCE), b.getPosition())
              case Some(Brake) =>
                if(Vec2.dot(v, u)  > 0){
                  b.applyForce(u.mul(- MAXBRAKE), b.getPosition())
                } else {
                  b.setLinearVelocity(zerovec)
                }
              case None => 
            }
          }


          
        }

      for(id <- toRemove){
        val bodyinfo= livebots.getOrElse(id, null)
        intents.remove(id)
        livebots.remove(id)
        world.destroyBody(bodyinfo.body)
        
      }
      
      controller ! 'StepDone
      controller ! ('Time, simulationTime)

      // draw it and step.
      background(0)
      val timestep = 1.0f / targetFPS
      world.step(timestep, 8)
      simulationTime += timestep
      


      dd.drawString(5, 30, "frames per second: " + avgFPS , new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(5, 42, "simulation time: " +  simulationTime, new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(5, 54, "spawn delay (adjust with left/right): " +  spawnDelay, new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(5, 66, "goals reached: " + numBotsDone, new Color3f(255.0f,255.0f,255.0f))


      dd.drawString(420, 560, "spawn prob. 0 (adjust with v/b): " +  spawnProbs(0), new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(25, 330, "spawn prob. 1 (adjust with a/s): " +  spawnProbs(1), new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(420, 40, "spawn prob. 2 (adjust with t/y): " +  spawnProbs(2), new Color3f(255.0f,255.0f,255.0f))
      dd.drawString(510, 278, "spawn prob. 3 (adjust with k/l): " +  spawnProbs(3), new Color3f(255.0f,255.0f,255.0f))

      return
    }
    


   val ticks = Array(spawnDelay, spawnDelay, spawnDelay, spawnDelay)


          

   val  rand = new scala.util.Random(System.currentTimeMillis())

    /**
     *  perhaps add bots to the world.
     */ 
    def perhapsCreateBots() : Unit = {

      for(i <- ticks.indices){
        if(ticks(i) > 0 ) {
          ticks.update(i,ticks(i) - 1)
        } else { 
          if(rand.nextDouble < spawnProbs(i) ){
            var spawn = spawnpoints(i)
            val v = spawnvels(i)
            val angle = spawnangles(i)
            val gl = goals(i)
            val omega = 0f;
            val checkdiag = spawnchecks(i).mul(6.0f)
            if( world.query(new AABB(spawn.sub(checkdiag), spawn.add(checkdiag)), 1).isEmpty) {
//              spawn = spawn.add(spawnbacksteps(i))
              val id = makeBot(spawn, v, gl, angle, omega )
              controller ! (('BotSpawn, id, spawn, v, angle, omega, i ))
              ticks.update(i,spawnDelay)
            }
          }
        }
      }


      return();
    }





    /*
     * Do I have to worry about thread safety here?
  */ 
    override def keyPressed() = {
      if(key == 'v') {
           spawnProbs(0) *= 1.25
           if(spawnProbs(0) > 1.0) {spawnProbs(0) = 1d}
      } else if(key == 'b') {
           spawnProbs(0) *= 0.8
      } else if(key == 'a') {
           spawnProbs(1) *= 1.25
           if(spawnProbs(1) > 1.0) {spawnProbs(1) = 1d}
      } else if(key == 's') {
           spawnProbs(1) *= 0.8
      } else if(key == 't') {
           spawnProbs(2) *= 1.25
           if(spawnProbs(2) > 1.0) {spawnProbs(2) = 1d}
      } else if(key == 'y') {
           spawnProbs(2) *= 0.8
      } else if(key == 'k') {
           spawnProbs(3) *= 1.25
           if(spawnProbs(3) > 1.0) {spawnProbs(3) = 1d}
      } else if(key == 'l') {
           spawnProbs(3) *= 0.8
      } else if(key == '1') {
           for(i<- spawnProbs.indices) {spawnProbs(i) = 1}
      } else if(keyCode == RIGHT) {
           spawnDelay -= 5
           if(spawnDelay < 0) {spawnDelay = 0}
      }  else if(keyCode == LEFT) {
           spawnDelay += 5
      }  


    }


}

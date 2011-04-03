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


class Physics extends PApplet {


    var shiftKey = false
    
    /** Was the mouse down last frame? */
    var pmousePressed = false
    
    /** FPS that we want to achieve */
    val targetFPS = 60.0f
    
    var avgFPS = 0.0
    val numFrames = 100
    var frameNum = 0
    var startTime : Long = System.currentTimeMillis()

    /** Drawing handler to use. */
    var dd: DebugDraw = null

    var world: World = null
    var controller: Controller = new Controller()

    
    var bodies : HashMap[BotID, Body]  = new HashMap[BotID,Body]()    
//    var positions : HashMap[BotID, Vec2]  = new HashMap[BotID,Vec2]()    
    var intents : SyncMap[BotID, Intent] = new SyncMap[BotID, Intent]()

    private var nextBotID = 0;


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


    def makeBot(p: Vec2, v: Vec2, theta:Float , omega: Float) = {
      val sd: PolygonDef = new PolygonDef()
      val a = 0.5f;
      sd.setAsBox(1.5f * a, a)
      sd.density = 5.0f
      sd.restitution = 0.0f
      sd.friction = 0.5f

      val bd: BodyDef = new BodyDef()
      bd.position.set(p)
      bd.linearDamping = 0.1f
      bd.angularDamping = 0.1f
      bd.angle = theta
      val body: Body = world.createBody(bd)
      body.createShape(sd)
      body.setMassFromShapes()
      body.setLinearVelocity(v)
      body.setAngularVelocity( omega)
      
      val bid = nextBotID
      bodies.put(bid, body)
      intents.put(bid,(None,Some(Accel)))
       
      nextBotID+= 1
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



    override def setup() {
    	size(800,600,P3D)
    	frameRate(targetFPS)
    	dd = new ProcessingDebugDraw(this)
        this.requestFocus()


        val worldAABB:AABB = new AABB()
        worldAABB.lowerBound = new Vec2(-200.0f, -100.0f)
        worldAABB.upperBound = new Vec2(200.0f, 200.0f)
        val gravity:Vec2 = new Vec2(0.0f, 0.0f)
        val doSleep = true
        world = new World(worldAABB, gravity, doSleep)
        world.setDebugDraw(dd)
    	
        dd.appendFlags(DebugDraw.e_shapeBit);
        dd.setCamera(0.0f,0.0f, 10.0f);

        // add some stuff to the world.

        makeBot(new Vec2(0.0f,0.0f),new Vec2(0.0f, 0.0f), 0.0f, 0.0f)

        makeBot(new Vec2(-8.0f,-8.0f),new Vec2(0.0f, 0.0f), 3.1415f/2.0f, 0.0f)

        makeBot(new Vec2(8.0f,8.0f),new Vec2(0.0f, 0.0f), 3.0f * 3.1415f/2.0f, 0.0f)

        makeBot(new Vec2(8.0f,-8.0f),new Vec2(0.0f, 0.0f),  3.1415f, 0.0f)


        makeBot(new Vec2(-8.0f,8.0f),new Vec2(0.0f, 0.0f),  0.0f, 0.0f)


        makeObstacle(new Vec2(-20.0f,0.0f), 6.0f)
        makeObstacle(new Vec2(20.0f,0.0f), 6.0f)

        makeObstacle(new Vec2(0.0f,-70.0f), 58.0f)
        makeObstacle(new Vec2(0.0f,70.0f), 58.0f)


        controller.papplet = this
        controller.start()
                                        
    }
    
    /**
     * This is the main looping function, and is called targetFPS times per second.
     */
    override def draw() {
        trackFPS()
        perhapsCreateBots()


        for( (id, intent) <- intents){
          val b = bodies.getOrElse(id, null)
          val theta = b.getAngle()
          val p = b.getPosition()
          val v = b.getLinearVelocity()
          val u = new Vec2(scala.math.cos(theta).asInstanceOf[Float], 
                           scala.math.sin(theta).asInstanceOf[Float])
          val (t,a) = intent
          controller ! ((id, p,v ))
          t match {
            case Some(TurnLeft) => b.applyTorque(1.0f)
            case Some(TurnRight) => b.applyTorque(-1.0f)
            case None => 

          }
          a match {
            case Some(Accel) => b.applyForce(u.mul(6.0f), b.getPosition())
            case Some(Brake) => b.applyForce(u.mul(-2.0f), b.getPosition())
            case None => 
          }

          
        }



        // draw it and step.
        background(0)
        world.step(1.0f / targetFPS, 8)


        dd.drawString(5, 30, "FPS: " + avgFPS ,new Color3f(255.0f,255.0f,255.0f))
        return
    }
    


    /**
     *  perhaps add bots to the world.
     */ 
    def perhapsCreateBots() : Unit = {
      return();
    }





    /*
     * Do I have to worry about thread safety here?
  */ 
    override def keyPressed(e: java.awt.event.KeyEvent) = {
      var (t,a) = intents.getOrElse(0,null)
      if(keyCode == UP) {
           a = Some(Accel)
      } else if(keyCode == DOWN) {
           a = Some(Brake)
      } else {
        a = None
      }


      if(keyCode == LEFT) {
           t = Some(TurnLeft)
      } else if(keyCode == RIGHT) {
           t = Some(TurnRight)
      } else {
        t = None
      }


      intents.put(0,(t,a))

    }



}

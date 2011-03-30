


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

class Physics extends PApplet {

    var shiftKey = false
    
    /** Was the mouse down last frame? */
    var pmousePressed = false
    
    
    
    /** FPS that we want to achieve */
    val targetFPS = 60.0f
    
    /** Drawing handler to use. */
    var dd: DebugDraw = null


    var world: World = null
    var controller: Controller = null

    
    override def setup() {
    	size(640,480,P3D)
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

        // add some stuff to the world.

        val sd: PolygonDef = new PolygonDef()
        val a = 0.5f;
        sd.setAsBox(a, a)
        sd.density = 5.0f
        sd.restitution = 0.0f
        sd.friction = 0.5f

        val bd: BodyDef = new BodyDef()
        bd.position.set(new Vec2(0.0f,0.0f))
        val body: Body = world.createBody(bd)
        body.createShape(sd)
        body.setMassFromShapes()
        body.setLinearVelocity(new Vec2(0.0f, 1.0f))

        body.setAngularVelocity( 0.2f)
        

        controller  = new Controller()
        controller.papplet = this
        controller.start()
                                        
    }
    
    /**
     * This is the main looping function, and is called targetFPS times per second.
     */
    override def draw() {
        createBots()


        // create a message and send it to controller.


        // draw it and step.
        background(0)
        world.step(1.0f / targetFPS, 8)
        return
    }
    

    /**
     *  perhaps add bots to the world.
     */ 
    def createBots() : Unit = {
      return();
    }



}

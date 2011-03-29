/*
 * JBox2D - A Java Port of Erin Catto's Box2D
 * 
 * JBox2D homepage: http://jbox2d.sourceforge.net/ 
 * Box2D homepage: http://www.box2d.org
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not
 * claim that you wrote the original software. If you use this software
 * in a product, an acknowledgment in the product documentation would be
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */



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
    
    
    // Processing handles fps pinning, but we
    // report fps on our own just to be sure.
    
    /** FPS that we want to achieve */
    val targetFPS = 60.0f
    /** Number of frames to average over when computing real FPS */
    val fpsAverageCount = 100
    
    /** Drawing handler to use. */
    var g: DebugDraw


    var world: World

    /** Constructor - real initialization happens in setup() function 
    def Physics() {
    	super()
    }
    *
  */ 
    
    /**
     * Called once upon program initialization (by Processing).
     * Here we set up graphics, set the framerate, register
     * all the testbed examples, set up a mousewheel listener,
     * and set up the frame rate timer.
     */
    override def setup() {
    	/* On newer machines especially, the default JAVA2D renderer
    	 * is slow as hell and tends to drop frames.  I have no idea
    	 * why, but for now let's use P3D and live without the smoothing...
    	 */
    	size(640,480,P3D)
    	frameRate(targetFPS)
    	g = new ProcessingDebugDraw(this)
    	//smooth();
        // what is this?
        this.requestFocus()


        val worldAABB:AABB = new AABB()
        worldAABB.lowerBound = new Vec2(-200.0f, -100.0f)
        worldAABB.upperBound = new Vec2(200.0f, 200.0f)
        val gravity:Vec2 = new Vec2(0.0f, 0.0f)
        val doSleep = true
        world = new World(worldAABB, gravity, doSleep)
        world.setDebugDraw(g)
    	
        g.appendFlags(DebugDraw.e_shapeBit);

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
        
                                        
    }
    
    /**
     * This is the main looping function, and is called targetFPS times per second.
     * In the testbed, Processing takes care of the timing of these calls for us,
     * but in your own game you will likely need to handle that yourself.  This function
     * also keeps detailed track of the current FPS and Vec2 creations for optimization
     * purposes.
     */
    override def draw() {
        background(0)
        world.step(1.0f / targetFPS, 8)
        g.drawString(5, 30, "Average FPS ("+fpsAverageCount+" frames)", new Color3f(255.0f,255.0f,255.0f))
        return
    }
    




}

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


import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;

import org.jbox2d.common.Vec2;
import org.jbox2d.collision.AABB;
import org.jbox2d.dynamics.DebugDraw;
import org.jbox2d.dynamics.World;

import processing.core.PApplet;


public class Visualizer extends PApplet {
    // TODO ewjordan: refactor into an input handler class
    /** Is the shift key held? */
    public boolean shiftKey = false;
    
    /** Was the mouse down last frame? */
    boolean pmousePressed = false;
    
    
    // Processing handles fps pinning, but we
    // report fps on our own just to be sure.
    
    /** FPS that we want to achieve */
    final static float targetFPS = 60.0f;
    /** Number of frames to average over when computing real FPS */
    final int fpsAverageCount = 100; 
    /** Array of timings */
    long[] nanos;
    /** When we started the nanotimer */
    long nanoStart; //
    
    /** Number of frames since we started this example. */
    long frameCount = 0;
    
    /** Drawing handler to use. */
    public DebugDraw g;


    public World world;

    /** Constructor - real initialization happens in setup() function */
    public Visualizer() {
    	super();
    }
    
    /**
     * Called once upon program initialization (by Processing).
     * Here we set up graphics, set the framerate, register
     * all the testbed examples, set up a mousewheel listener,
     * and set up the frame rate timer.
     */
    public void setup() {
    	/* On newer machines especially, the default JAVA2D renderer
    	 * is slow as hell and tends to drop frames.  I have no idea
    	 * why, but for now let's use P3D and live without the smoothing...
    	 */
    	size(640,480,P3D);
    	frameRate(targetFPS);
    	g = new ProcessingDebugDraw(this);
    	//smooth();
    	for (int i=0; i<100; ++i) {
    		this.requestFocus();
    	}
    	
    	/* Set up the timers for FPS reporting */
    	nanos = new long[fpsAverageCount];
    	long nanosPerFrameGuess = (long)(1000000000.0 / targetFPS);
    	nanos[fpsAverageCount-1] = System.nanoTime();
    	for (int i=fpsAverageCount-2; i>=0; --i) {
    		nanos[i] = nanos[i+1] - nanosPerFrameGuess;
    	}
    	nanoStart = System.nanoTime();


        AABB worldAABB = new AABB();
        worldAABB.lowerBound = new Vec2(-200.0f, -100.0f);
        worldAABB.upperBound = new Vec2(200.0f, 200.0f);
        Vec2 gravity = new Vec2(0.0f, 0.0f);
        boolean doSleep = true;
        world = new World(worldAABB, gravity, doSleep);
        world.setDebugDraw(g);
    	
    }
    
    /**
     * This is the main looping function, and is called targetFPS times per second.
     * In the testbed, Processing takes care of the timing of these calls for us,
     * but in your own game you will likely need to handle that yourself.  This function
     * also keeps detailed track of the current FPS and Vec2 creations for optimization
     * purposes.
     */
    public void draw() {
        
        world.step(1.0f / targetFPS, 8);
        return;
    }
    
    /** Just report a mouseMoved event if mouseDragged. */
    public void mouseDragged() {
    	mouseMoved();
    }
    
    public void keyPressed() {
        return;
    }
    
    /** Handle keyReleased events and pass them on to currentTest. */
    public void keyReleased() {
    	if (keyCode == PApplet.SHIFT) {
            shiftKey = false;
        }
        return;
    }
    


    /** Start PApplet as a Java program (can also be run as an applet). */
    static public void main(String args[]) {
        PApplet.main(new String[] { "Visualizer" });
    }

}

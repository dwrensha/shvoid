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


class StopSign extends Physics {
  override def setup() {
    println("in stopsign")
    controller = new StopSignController(intents, 
                                        MAXFORCE / BOTMASS, 
                                        MAXBRAKE / BOTMASS,
                                        EPS)
   super.setup()
  }


}

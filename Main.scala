//package shvoid

import processing.core.PApplet

object Main {
  def main (args : Array[String]) : Unit = {
    println("hello")
    println("Main args = " + args)
//    val p = new Physics()
//    PApplet.runSketch(args, p)
    PApplet.main(Array[String] { "Physics" })
    println("goodbye")
  }
}

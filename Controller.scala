//package shvoid

import scala.actors.Actor
import scala.actors.Actor._


class Controller extends Actor {

  var papplet : processing.core.PApplet = null;

  

  def act() : Unit = {
    println("hello from controller")



    while(true) {

      // get everything from mailbox
      receive {
        case ('BotSpawn, id, spawn, v, gl, angle, omega ) =>
          println("a bot spawned.")
        case ('BotDone, id ) =>
          println("a bot reached its goal.")
        case msg => 
          println("got message: " + msg)

      }
      

      // update





    }


  }


}

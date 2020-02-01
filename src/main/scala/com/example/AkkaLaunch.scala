//#full-example
package com.example


import akka.actor.typed.ActorSystem
import com.example.Action.{ActorAction, CreatePopulation}

//#main-class
object AkkaLaunch extends App {
  val city: ActorSystem[ActorAction] = ActorSystem(City(), "AkkaQuickStart")
  //#actor-system

  city ! CreatePopulation
  //#main-send-messages
}
//#main-class
//#full-example

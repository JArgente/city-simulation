package com.quantictime.citysim

import akka.actor.typed.ActorSystem
import com.quantictime.citysim.Action.{ActorAction, CreatePopulation}

object CitySim extends App {
  val city: ActorSystem[ActorAction] = ActorSystem(City(), "City_simulation")

  city ! CreatePopulation
}

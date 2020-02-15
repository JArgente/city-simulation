package com.quantictime.citysim

import akka.actor.typed.ActorRef
import com.quantictime.citysim.City.CityInfo

object Action {

  sealed trait ActorAction
  final case class BlacksmithAction(quality: Int, price: Int, closed: Boolean, replyTo: ActorRef[CityInfo] ) extends ActorAction
  final case class GuardAction(quality: Int, price: Int, closed: Boolean, replyTo: ActorRef[CityInfo] ) extends ActorAction
  final case class TavernAction(quality: Int, price: Int, closed: Boolean, replyTo: ActorRef[CityInfo] ) extends ActorAction
  object CreatePopulation extends ActorAction
}
package com.quantictime.citysim

import akka.actor.typed.scaladsl.ActorContext

trait Citizen {

  def who():String

  def performAction(context: ActorContext[City.CityInfo], duration:Long, action: String): Unit={
    context.log.info(who()+" starts "+action)
    Thread.sleep(duration)
    context.log.info(who()+" ends "+action)
  }

}

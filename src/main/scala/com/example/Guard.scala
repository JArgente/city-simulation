package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.Action.{BlacksmithAction, GuardAction}
import com.example.ActorInfo.{Personality, Skill, Status}
import com.example.City.CityInfo

import scala.util.Random

object Guard {

  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Guard(context,
      ActorInfo(List(Skill("sword",3)),
        Personality(4,6,2,2,5),
        Status(2,3,5,1),10),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Guard(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) {

  override def onMessage(msg: City.CityInfo): Behavior[City.CityInfo] = {
    msg match {
      case City.CityInfo(lifeQuality, richness, population, merchantActivity, tavernOpen, replyTo) =>
        val newStatusInfo = newStatus(msg, actorInfo, timeMultiplyer)
        routine(newStatusInfo, msg)
        replyTo ! getNextDay(msg, newStatusInfo)
        context.log.info("GUARDS MONEY: "+newStatusInfo.money)
        Behaviors.setup(context => new Guard(context,
          newStatusInfo, timeMultiplyer))
    }
  }

  private def routine(status: ActorInfo, city: CityInfo): Unit ={
    context.log.info("Guard wakes up")
    context.log.info("Guard starts working...")
    Thread.sleep(15*timeMultiplyer)
    context.log.info("Guard ends his work")

    if(status.money >= 5 && city.tavernOpen) {
      context.log.info("Guard goes to tavern")
      Thread.sleep(3*timeMultiplyer)
      context.log.info("Guard exits the tavern")
    }else{
      context.log.info("Guard goes to rest")
      Thread.sleep(3*timeMultiplyer)
      context.log.info("Guard ends his rest")
    }
    context.log.info("Guard goes to sleep")
    Thread.sleep(12*timeMultiplyer)
  }

  private def getNextDay(cityInfo: City.CityInfo, actorInfo: ActorInfo): GuardAction = {
    if(cityInfo.population > 7) {
      val death=Random.nextInt(10)
      if(death >=5)
        context.log.info("Guard dies...")
        GuardAction(0,0,true,this.context.self)
    }
    GuardAction(0,0,false,this.context.self)
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo, timeMultiplyer:Int): ActorInfo = {
    if(cityInfo.population > 6)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear,
          actorInfo.personality.entrepeneur+1,
          actorInfo.personality.lovee,
          actorInfo.personality.selfish,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious-1,
          actorInfo.status.wealth+1,
          actorInfo.status.wellness+1,
          actorInfo.status.fame),
        actorInfo.money+2
      )
    else if(cityInfo.population < 4)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear-1,
          actorInfo.personality.entrepeneur,
          actorInfo.personality.lovee,
          actorInfo.personality.selfish+1,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious,
          actorInfo.status.wealth+2,
          actorInfo.status.wellness+1,
          actorInfo.status.fame+1),
        actorInfo.money-2
      )
    else
      actorInfo

  }

}

package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.Action.{BlacksmithAction, TavernAction}
import com.example.ActorInfo.{Personality, Skill, Status}

import scala.util.Random

object Tavern {

  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Tavern(context,
      ActorInfo(List(Skill("smith",3)),
        Personality(4,6,2,2,5),
        Status(2,3,5,1),
        10),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Tavern(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) {

  override def onMessage(msg: City.CityInfo): Behavior[City.CityInfo] = {
    msg match {
      case City.CityInfo(lifeQuality, richness, population, merchantActivity, tavernOpen, replyTo) =>
        val newStatusInfo = newStatus(msg, actorInfo)
        routine()
        replyTo ! getNextDay(msg, newStatusInfo, timeMultiplyer)
        context.log.info("TAVERN MONEY: "+newStatusInfo.money)
        Behaviors.setup(context => new Tavern(context,
          newStatusInfo, timeMultiplyer))
    }
  }

  private def routine(): Unit ={
    context.log.info("Tavern wakes up")
    context.log.info("Tavern starts working...")
    Thread.sleep(9*timeMultiplyer)
    context.log.info("Tavern stops his work")
    context.log.info("Tavern goes to rest")
    Thread.sleep(3*timeMultiplyer)
    context.log.info("Tavern finish the rest")
    context.log.info("Tavern starts working...")
    Thread.sleep(9*timeMultiplyer)
    context.log.info("Tavern finish his work")
    context.log.info("Tavern goes to sleep")
    Thread.sleep(9*timeMultiplyer)
  }

  private def getNextDay(cityInfo: City.CityInfo, actorInfo: ActorInfo, timeMultiplyer:Int): TavernAction = {
    if(actorInfo.money>7) {
      context.log.info("Tavern raises prices...")
      TavernAction(1,1,false,this.context.self)
    } else if(actorInfo.money > 4) {
      TavernAction(1,0,false,this.context.self)
    } else if(actorInfo.money <= 1) {
      context.log.info("Tavern closes...")
      TavernAction(0,0,true,this.context.self)
    }else {
      context.log.info("Tavern lowers prices...")
      TavernAction(0, -1, false, this.context.self)
    }
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo): ActorInfo = {
    if(cityInfo.population > 5)
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
        actorInfo.money+Random.nextInt(cityInfo.population)
      )
    else if(cityInfo.population < 3)
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

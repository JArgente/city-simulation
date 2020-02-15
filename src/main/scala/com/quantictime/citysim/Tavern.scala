package com.quantictime.citysim

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.quantictime.citysim.Action.TavernAction
import com.quantictime.citysim.ActorInfo.{Personality, Skill, Status}

import scala.util.Random

object Tavern {

  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Tavern(context,
      ActorInfo(List(Skill("smith",3)),
        Personality(4,6),
        Status(2,3),
        10),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Tavern(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) with Citizen {

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
    performAction(context,9*timeMultiplyer,"working")
    performAction(context,3*timeMultiplyer,"going to rest")
    performAction(context,9*timeMultiplyer,"working")
    performAction(context,9*timeMultiplyer,"going to sleep")
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
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious-1,
          actorInfo.status.fame),
        actorInfo.money+Random.nextInt(cityInfo.population)
      )
    else if(cityInfo.population < 3)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear-1,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious,
          actorInfo.status.fame+1),
        actorInfo.money-2
      )
    else
      actorInfo

  }

  override def who(): String = "Tavern"
}

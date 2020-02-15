package com.quantictime.citysim

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.quantictime.citysim.Action.BlacksmithAction
import com.quantictime.citysim.ActorInfo.{Personality, Skill, Status}
import com.quantictime.citysim.City.CityInfo

import scala.util.Random

object Blacksmith {
  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Blacksmith(context,
      ActorInfo(List(Skill("smith",3)),
        Personality(4,6),
        Status(2,3),10),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Blacksmith(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) with Citizen {

  override def onMessage(msg: City.CityInfo): Behavior[City.CityInfo] = {
    msg match {
      case City.CityInfo(lifeQuality, richness, population, merchantActivity, tavernOpen, replyTo) =>
        val newStatusInfo= newStatus(msg, actorInfo)
        routine(newStatusInfo, msg)
        replyTo ! getNextDay(msg, newStatusInfo)
        context.log.info("SMITH MONEY: "+newStatusInfo.money)
        Behaviors.setup(context => new Blacksmith(context, newStatusInfo, timeMultiplyer))
    }
  }

  private def getNextDay(cityInfo: City.CityInfo, actorInfo: ActorInfo): BlacksmithAction = {
      if(actorInfo.money>7) {
        context.log.info("Smithers raises prices...")
        BlacksmithAction(1,1,false,this.context.self)
      } else if(actorInfo.money > 4) {
        BlacksmithAction(1,0,false,this.context.self)
      } else if(actorInfo.money <= 1) {
        context.log.info("Smithers closes...")
        BlacksmithAction(0,0,true,this.context.self)
      }else {
        context.log.info("Smithers lowers prices...")
        BlacksmithAction(0, -1, false, this.context.self)
      }
  }

  private def routine(status: ActorInfo, city: CityInfo): Unit ={
    performAction(context,12*timeMultiplyer,"Working")
    context.log.info("Smithers wakes up")
    if(status.money >= 5 && city.tavernOpen) {
      performAction(context,6*timeMultiplyer,"going tavern")
    }else{
      performAction(context,6*timeMultiplyer,"going to rest")
    }
    performAction(context,6*timeMultiplyer,"going to sleep")
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo): ActorInfo = {
    if(cityInfo.population > 7)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious-1,
          actorInfo.status.fame),
        actorInfo.money+Random.nextInt(cityInfo.population)
      )
    else if(cityInfo.population < 4)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear-1,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious,
          actorInfo.status.fame+1),
        actorInfo.money-4-Random.nextInt(cityInfo.population)
      )
    else
      actorInfo

  }

  override def who(): String = "Blacksmith"
}
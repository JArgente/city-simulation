package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.Action.BlacksmithAction
import com.example.ActorInfo.{Personality, Skill, Status}
import com.example.City.CityInfo

import scala.util.Random

object Blacksmith {
  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Blacksmith(context,
      ActorInfo(List(Skill("smith",3)),
        Personality(4,6,2,2,5),
        Status(2,3,5,1),10),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Blacksmith(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) {

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
    context.log.info("Smithers wakes up")
    context.log.info("Smithers starts working...")
    Thread.sleep(12*timeMultiplyer)
    context.log.info("Smithers ends his work")
    if(status.money >= 5 && city.tavernOpen) {
      context.log.info("Smithers goes to tavern")
      Thread.sleep(6*timeMultiplyer)
      context.log.info("Smithers exits the tavern")
    }else{
      context.log.info("Smithers goes to rest")
      Thread.sleep(6*timeMultiplyer)
      context.log.info("Smithers ends his rest")
    }
    context.log.info("Smithers goes to sleep")
    Thread.sleep(12*timeMultiplyer)
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo): ActorInfo = {
    if(cityInfo.population > 7)
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
        actorInfo.money-4-Random.nextInt(cityInfo.population)
      )
    else
      actorInfo

  }

}
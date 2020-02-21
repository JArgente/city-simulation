package com.quantictime.citysim

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.quantictime.citysim.Action.GuardAction
import com.quantictime.citysim.ActorInfo.{Personality, Skill, Status}
import com.quantictime.citysim.City.{CityInfo, Mission}

import scala.util.Random

object Guard {

  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Guard(context,
      ActorInfo(List(Skill("sword",3)),
        Personality(4,6),
        Status(2,3), None, 10, false),
      context.system.settings.config.getInt("timeMultiplyer")))
}

class Guard(context: ActorContext[City.CityInfo], actorInfo: ActorInfo, timeMultiplyer:Int)
  extends AbstractBehavior[City.CityInfo](context) with Citizen {

  override def onMessage(msg: City.CityInfo): Behavior[City.CityInfo] = {
    msg match {
      case City.CityInfo(lifeQuality, richness, population, merchantActivity, tavernOpen, missions, replyTo) =>

      if (actorInfo.mission.isDefined) {
          context.log.info ("Guard Continues in a mission: " + actorInfo.mission.get.duration + " days left")
          val newStatusInfo =
          if (actorInfo.mission.get.duration > 1) {
              ActorInfo (
              actorInfo.skills,
              Personality (actorInfo.personality.fear,
              actorInfo.personality.greed),
              Status (actorInfo.status.tedious,
              actorInfo.status.fame),
              Some (Mission (
                actorInfo.mission.get.danger,
                actorInfo.mission.get.reward,
                actorInfo.mission.get.duration - 1
              ) ),
              actorInfo.money,
              Random.nextInt (10) <= actorInfo.mission.get.danger
              )
          } else {
              context.log.info ("Guard Return from the adventure ")
              ActorInfo (
              actorInfo.skills,
              Personality (actorInfo.personality.fear,
              actorInfo.personality.greed),
              Status (actorInfo.status.tedious,
              actorInfo.status.fame),
              None,
              actorInfo.money + actorInfo.mission.get.reward,
              Random.nextInt (10) < 1
              )
          }
          Behaviors.setup (context => new Blacksmith (context, newStatusInfo, timeMultiplyer) )
    }
      else {
        val newStatusInfo = newStatus(msg, actorInfo)
        routine(newStatusInfo, msg)
        replyTo ! getNextDay(msg, newStatusInfo)
        context.log.info("SMITH MONEY: " + newStatusInfo.money)
        Behaviors.setup(context => new Blacksmith(context, newStatusInfo, timeMultiplyer))
      }
    }
  }

  private def routine(status: ActorInfo, city: CityInfo): Unit ={
    context.log.info("Guard wakes up")
    performAction(context,15*timeMultiplyer,"working")

    if(status.money >= 5 && city.tavernOpen) {
      performAction(context,3*timeMultiplyer,"going tavern")
    }else{
      performAction(context,3*timeMultiplyer,"going to rest")
    }
    performAction(context,12*timeMultiplyer,"going to sleep")
  }

  private def getNextDay(cityInfo: City.CityInfo, actorInfo: ActorInfo): GuardAction = {
    if(cityInfo.population > 7) {
      val death=Random.nextInt(10)
      if(death >=5)
        context.log.info("Guard dies...")
        GuardAction(0,0,true,None,this.context.self)
    }
    GuardAction(0,0,false,None,this.context.self)
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo): ActorInfo = {
    val newInfo =
    if(cityInfo.population > 6)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious-1,
          actorInfo.status.fame),
          scala.None,
        actorInfo.money + Random.nextInt(cityInfo.population),
        Random.nextInt(10)<1
      )
    else if(cityInfo.population < 4)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear-1,
          actorInfo.personality.greed+1),
        Status(actorInfo.status.tedious,
          actorInfo.status.fame+1),
        None,
        actorInfo.money - 4 - Random.nextInt(cityInfo.population),
        Random.nextInt(10)<1
      )
    else
      actorInfo
    if (newInfo.money <= 1 && cityInfo.misions.nonEmpty && cityInfo.misions.minBy(_.danger).danger >= actorInfo.personality.fear) {
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear,
          actorInfo.personality.greed),
        Status(actorInfo.status.tedious,
          actorInfo.status.fame),
        Some(cityInfo.misions.minBy(_.danger)),
        actorInfo.money,
        Random.nextInt(10)<1
      )
    }else{
      newInfo
    }
  }

  override def who(): String = "Guard"
}

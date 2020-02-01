package com.example

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.Action.{BlacksmithAction, GuardAction}
import com.example.ActorInfo.{Personality, Skill, Status}

object Guard {

  def apply(): Behavior[City.CityInfo] =
    Behaviors.setup(context => new Blacksmith(context,
      ActorInfo(List(Skill("sword",3)),
        Personality(4,6,2,2,5),
        Status(2,3,5,1))))
}

class Guard(context: ActorContext[City.CityInfo], actorInfo: ActorInfo)
  extends AbstractBehavior[City.CityInfo](context) {

  override def onMessage(msg: City.CityInfo): Behavior[City.CityInfo] = {
    msg match {
      case City.CityInfo(lifeQuality, richness, population, merchantActivity, replyTo) =>
        context.log.info("Guard message {}", msg)
        replyTo ! getNextDay(msg, actorInfo)
        Behaviors.setup(context => new Blacksmith(context,
          newStatus(msg, actorInfo)))
    }
  }

  private def getNextDay(cityInfo: City.CityInfo, actorInfo: ActorInfo): GuardAction = {
    if(actorInfo.status.wealth<3) {
      GuardAction(1,1,this.context.self)
    } else if(actorInfo.personality.fear < 2) {
      if(actorInfo.personality.greed >7)
        GuardAction(1,1,this.context.self)
      else
        GuardAction(1,0,this.context.self)
    } else
      GuardAction(0,1,this.context.self)
  }

  private def newStatus(cityInfo: City.CityInfo, actorInfo: ActorInfo): ActorInfo = {
    if(cityInfo.population < 4)
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
          actorInfo.status.fame)
      )
    else if(cityInfo.richness > 7)
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
          actorInfo.status.fame+1)
      )
    else if(cityInfo.lifeQuality > 8)
      ActorInfo(
        actorInfo.skills,
        Personality(actorInfo.personality.fear-2,
          actorInfo.personality.entrepeneur-1,
          actorInfo.personality.lovee+1,
          actorInfo.personality.selfish,
          actorInfo.personality.greed),
        Status(actorInfo.status.tedious-1,
          actorInfo.status.wealth+1,
          actorInfo.status.wellness+2,
          actorInfo.status.fame)
      )
    else
      actorInfo

  }

}

package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.example.Action._
import com.example.City.CityInfo

object City {
  case class CityInfo(
                     lifeQuality: Int,
                     richness: Int,
                     population: Int,
                     merchantActivity: Int,
                     replyTo: ActorRef[ActorAction]
                     )
  def apply(): Behavior[ActorAction] =
    Behaviors.setup(context => new City(context,
      City.CityInfo(5,6,3,4,null)))

}

class City(context: ActorContext[ActorAction], cityInfo: CityInfo)
  extends AbstractBehavior[ActorAction](context) {
  override def onMessage(msg: ActorAction): Behavior[ActorAction] = {
    msg match {
      case BlacksmithAction(quality, price, replyTo) =>
        val newCityInfo = getNewBlacksmithCityInfo(cityInfo, BlacksmithAction(quality, price, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case GuardAction(quality, price, replyTo) =>
        val newCityInfo = getNewGuardCityInfo(cityInfo, GuardAction(quality, price, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case TavernAction(quality, price, replyTo) =>
        val newCityInfo = getNewTavernCityInfo(cityInfo, TavernAction(quality, price, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))

      case CreatePopulation =>
        val blacksmith = context.spawn(Blacksmith(), s"blacksmith")
        val guard = context.spawn(Guard(), s"guard")
        val tavern = context.spawn(Tavern(), s"tavern")
        val message = CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity, this.context.self)
        blacksmith ! message
        guard ! message
        tavern ! message
        this
    }
  }

  private def getNewBlacksmithCityInfo(cityInfo: CityInfo, blacksmithAction: BlacksmithAction): CityInfo = {
    if(blacksmithAction.price > 0)
      CityInfo(cityInfo.lifeQuality, cityInfo.richness+1, cityInfo.population, cityInfo.merchantActivity+1, this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity, this.context.self)
  }

  private def getNewGuardCityInfo(cityInfo: CityInfo, guardAction: GuardAction): CityInfo = {
    if(guardAction.quality > 0)
      CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+1, cityInfo.merchantActivity, this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity, this.context.self)
  }

  private def getNewTavernCityInfo(cityInfo: CityInfo, tavernAction: TavernAction): CityInfo = {
    if(tavernAction.quality > 0)
      CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+1, cityInfo.merchantActivity+1, this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity, this.context.self)
  }
}


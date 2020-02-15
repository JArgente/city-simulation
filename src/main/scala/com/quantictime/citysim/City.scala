package com.quantictime.citysim

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.quantictime.citysim.Action._
import com.quantictime.citysim.City.CityInfo

import scala.util.Random

object City {
  case class CityInfo(
                     lifeQuality: Int,
                     richness: Int,
                     population: Int,
                     merchantActivity: Int,
                     tavernOpen: Boolean,
                     replyTo: ActorRef[ActorAction]
                     )
  def apply(): Behavior[ActorAction] =
    Behaviors.setup(context => new City(context,
      City.CityInfo(8,6,5,4, true, null)))

}

class City(context: ActorContext[ActorAction], cityInfo: CityInfo)
  extends AbstractBehavior[ActorAction](context) {
  override def onMessage(msg: ActorAction): Behavior[ActorAction] = {
    msg match {
      case BlacksmithAction(quality, price, closed, replyTo) =>
        val newCityInfo = getNewBlacksmithCityInfo(cityInfo, BlacksmithAction(quality, price, closed, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case GuardAction(quality, price, closed, replyTo) =>
        val newCityInfo = getNewGuardCityInfo(cityInfo, GuardAction(quality, price, closed, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case TavernAction(quality, price, closed, replyTo) =>
        val newCityInfo = getNewTavernCityInfo(cityInfo, TavernAction(quality, price, closed, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))

      case CreatePopulation =>
        val blacksmith = context.spawn(Blacksmith(), s"blacksmith")
        val guard = context.spawn(Guard(), s"guard")
        val tavern = context.spawn(Tavern(), s"tavern")
        val message = CityInfo (cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity, true, this.context.self)
        blacksmith ! message
        guard ! message
        tavern ! message
        this
    }
  }

  private def getNewBlacksmithCityInfo(cityInfo: CityInfo, blacksmithAction: BlacksmithAction): CityInfo = {
    if(blacksmithAction.closed)
      context.stop(blacksmithAction.replyTo)

    if(blacksmithAction.price > 0)
      CityInfo(cityInfo.lifeQuality, cityInfo.richness+1, cityInfo.population-Random.nextInt(2), cityInfo.merchantActivity+1, cityInfo.tavernOpen, this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population+Random.nextInt(2), cityInfo.merchantActivity, cityInfo.tavernOpen, this.context.self)
  }

  private def getNewGuardCityInfo(cityInfo: CityInfo, guardAction: GuardAction): CityInfo = {
    if(guardAction.closed)
      context.stop(guardAction.replyTo)

    if(guardAction.quality > 0)
      CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+Random.nextInt(3), cityInfo.merchantActivity, cityInfo.tavernOpen, this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population-Random.nextInt(3), cityInfo.merchantActivity, cityInfo.tavernOpen, this.context.self)
  }

  private def getNewTavernCityInfo(cityInfo: CityInfo, tavernAction: TavernAction): CityInfo = {
    if(tavernAction.closed)
      context.stop(tavernAction.replyTo)

    CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+1, cityInfo.merchantActivity+1, !tavernAction.closed, this.context.self)
  }
}


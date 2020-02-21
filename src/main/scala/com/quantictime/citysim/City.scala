package com.quantictime.citysim

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import com.quantictime.citysim.Action._
import com.quantictime.citysim.City.{CityInfo, Mission}

import scala.util.Random

object City {
  case class CityInfo(
                     lifeQuality: Int,
                     richness: Int,
                     population: Int,
                     merchantActivity: Int,
                     tavernOpen: Boolean,
                     misions: List[Mission],
                     replyTo: ActorRef[ActorAction]
                     )
  case class Mission(
                   danger: Int,
                   reward: Int,
                   duration:Int
                   )

  def apply(): Behavior[ActorAction] =
    Behaviors.setup(context => new City(context,
      City.CityInfo(8,6,5,4, true, List.empty, null)))

}

class City(context: ActorContext[ActorAction], cityInfo: CityInfo)
  extends AbstractBehavior[ActorAction](context) {
  override def onMessage(msg: ActorAction): Behavior[ActorAction] = {
    msg match {
      case BlacksmithAction(quality, price, closed, mission, replyTo) =>
        val newCityInfo = getNewBlacksmithCityInfo(cityInfo, BlacksmithAction(quality, price, closed, mission, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case GuardAction(quality, price, closed, mission, replyTo) =>
        val newCityInfo = getNewGuardCityInfo(cityInfo, GuardAction(quality, price, closed, mission, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))
      case TavernAction(quality, price, closed, mission, replyTo) =>
        val newCityInfo = getNewTavernCityInfo(cityInfo, TavernAction(quality, price, closed, mission, replyTo))
        replyTo ! newCityInfo
        Behaviors.setup(context => new City(context,
          newCityInfo))

      case CreatePopulation =>
        val blacksmith = context.spawn(Blacksmith(), s"blacksmith")
        val guard = context.spawn(Guard(), s"guard")
        val tavern = context.spawn(Tavern(), s"tavern")
        val message = CityInfo (cityInfo.lifeQuality, cityInfo.richness, cityInfo.population, cityInfo.merchantActivity,  true, List(getRandomMissions()), this.context.self)
        blacksmith ! message
        guard ! message
        tavern ! message
        this
    }
  }

  private def getRandomMissions(): Mission ={
    if(Random.nextBoolean()){
      Mission(Random.nextInt(9),Random.nextInt(9),Random.nextInt(9))
    }
    null
  }

  private def getNewBlacksmithCityInfo(cityInfo: CityInfo, blacksmithAction: BlacksmithAction): CityInfo = {
    if(blacksmithAction.closed)
      context.stop(blacksmithAction.replyTo)

    if(blacksmithAction.price > 0)
      CityInfo(cityInfo.lifeQuality, cityInfo.richness+1, cityInfo.population-Random.nextInt(2), cityInfo.merchantActivity+1, cityInfo.tavernOpen, cityInfo.misions:+getRandomMissions(), this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population+Random.nextInt(2), cityInfo.merchantActivity, cityInfo.tavernOpen, cityInfo.misions:+getRandomMissions(), this.context.self)
  }

  private def getNewGuardCityInfo(cityInfo: CityInfo, guardAction: GuardAction): CityInfo = {
    if(guardAction.closed)
      context.stop(guardAction.replyTo)

    if(guardAction.quality > 0)
      CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+Random.nextInt(3), cityInfo.merchantActivity, cityInfo.tavernOpen, cityInfo.misions:+getRandomMissions(), this.context.self)
    else
      CityInfo(cityInfo.lifeQuality, cityInfo.richness, cityInfo.population-Random.nextInt(3), cityInfo.merchantActivity, cityInfo.tavernOpen, cityInfo.misions:+getRandomMissions(), this.context.self)
  }

  private def getNewTavernCityInfo(cityInfo: CityInfo, tavernAction: TavernAction): CityInfo = {
    if(tavernAction.closed)
      context.stop(tavernAction.replyTo)

    CityInfo(cityInfo.lifeQuality+1, cityInfo.richness, cityInfo.population+1, cityInfo.merchantActivity+1, !tavernAction.closed, cityInfo.misions:+getRandomMissions(), this.context.self)
  }


}


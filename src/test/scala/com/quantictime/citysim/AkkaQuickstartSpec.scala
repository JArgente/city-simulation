package com.quantictime.citysim

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import Action._
import City.CityInfo
import org.scalatest.WordSpecLike

class AkkaQuickstartSpec extends ScalaTestWithActorTestKit with WordSpecLike {

  "A Blacksmith" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Blacksmith())
      underTest ! CityInfo(5,3,3,6, true, replyProbe.ref)
      replyProbe.expectMessage(BlacksmithAction(1,0, false, underTest.ref))
    }
  }

  "A Guard" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Guard())
      underTest ! CityInfo(5,3,3,6, true, replyProbe.ref)
      replyProbe.expectMessage(GuardAction(0,0, false, underTest.ref))
    }
  }

  "A Tavern" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Tavern())
      underTest ! CityInfo(5,3,3,6, true, replyProbe.ref)
      replyProbe.expectMessage(TavernAction(1,1, false, underTest.ref))
    }
  }

}
package com.example

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import com.example.Action._
import com.example.City.CityInfo
import org.scalatest.WordSpecLike

class AkkaQuickstartSpec extends ScalaTestWithActorTestKit with WordSpecLike {

  "A Blacksmith" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Blacksmith())
      underTest ! CityInfo(5,3,3,6, replyProbe.ref)
      replyProbe.expectMessage(BlacksmithAction(1,0, underTest.ref))
    }
  }

  "A Guard" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Guard())
      underTest ! CityInfo(5,3,3,6, replyProbe.ref)
      replyProbe.expectMessage(GuardAction(0,1, underTest.ref))
    }
  }

  "A Tavern" must {
    "reply to city" in {
      val replyProbe = createTestProbe[ActorAction]()
      val underTest = spawn(Tavern())
      underTest ! CityInfo(5,3,3,6, replyProbe.ref)
      replyProbe.expectMessage(TavernAction(1,0, underTest.ref))
    }
  }

}
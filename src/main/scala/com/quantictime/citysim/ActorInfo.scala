package com.quantictime.citysim

import com.quantictime.citysim.ActorInfo.{Personality, Skill, Status}
import com.quantictime.citysim.City.Mission

object ActorInfo {

  final case class Skill(name: String, expertise: Int)
  final case class Personality(
                                fear: Int,
                                greed: Int
                              )
  final case class Status(
                           tedious: Int,
                           fame: Int
                         )

}

case class ActorInfo(skills: List[Skill],
                     personality: Personality,
                     status: Status,
                     mission: Option[Mission],
                     money: Int,
                     dead: Boolean
                     )

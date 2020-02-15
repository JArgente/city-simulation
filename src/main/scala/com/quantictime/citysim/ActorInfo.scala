package com.quantictime.citysim

import com.quantictime.citysim.ActorInfo.{Personality, Skill, Status}

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
                     money: Int)

package com.example

import com.example.ActorInfo.{Personality, Skill, Status}

object ActorInfo {

  final case class Skill(name: String, expertise: Int)
  final case class Personality(
                                fear: Int,
                                entrepeneur: Int,
                                lovee: Int,
                                selfish: Int,
                                greed: Int
                              )
  final case class Status(
                           tedious: Int,
                           wealth: Int,
                           wellness: Int,
                           fame: Int
                         )

}

case class ActorInfo(skills: List[Skill],
                     personality: Personality,
                     status: Status)

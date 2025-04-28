package com.keithalcock.aoc.year2021.day23

import com.keithalcock.aoc.Test
import com.keithalcock.aoc.year2021.day23.Part1._

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 44169
    // val actualResult = Part2.run("com/keithalcock/aoc/year2021/day23/test.txt")
    //  #D#C#B#A#
    //  #D#B#A#C#
    val occupants = Seq(
      A, D, D, B,
      D, B, C, C,
      C, A, B, B,
      A, C, A, D,
      W, E, E, E, E, E, E, E, E, E, E, E, W
    )
    val actualResult = Part2.run(occupants)

    actualResult should be (expectedResult)
  }
}

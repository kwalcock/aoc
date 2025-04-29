package com.keithalcock.aoc.year2021.day24

import com.keithalcock.aoc.Test
import com.keithalcock.aoc.year2021.day23.Part1._

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 12521
    // val actualResult = Part1.run("com/keithalcock/aoc/year2021/day23/test2.txt")
    val occupants = Seq(A, B, D, C, C, B, A, D, W, E, E, E, E, E, E, E, E, E, E, E, W)
    val actualResult = Part1.run(occupants)

    actualResult should be (expectedResult)
  }
}

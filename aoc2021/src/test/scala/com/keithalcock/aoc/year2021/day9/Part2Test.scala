package com.keithalcock.aoc.year2021.day9

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 1134
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day9/test.txt")

    actualResult should be (expectedResult)
  }
}

package com.keithalcock.aoc.year2021.day7

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 168
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day7/test.txt")

    expectedResult should be (actualResult)
  }
}

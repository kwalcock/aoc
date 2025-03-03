package com.keithalcock.aoc.year2021.day3

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 230
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day3/test.txt")

    expectedResult should be (actualResult)
  }
}

package com.keithalcock.aoc.year2021.day4

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 1924
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day4/test.txt")

    actualResult should be (expectedResult)
  }
}

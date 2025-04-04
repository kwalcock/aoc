package com.keithalcock.aoc.year2021.day10

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 288957
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day10/test.txt")

    actualResult should be (expectedResult)
  }
}

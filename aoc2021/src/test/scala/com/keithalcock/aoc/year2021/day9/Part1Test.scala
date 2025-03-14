package com.keithalcock.aoc.year2021.day9

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 15
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day9/test.txt")

    expectedResult should be (actualResult)
  }
}

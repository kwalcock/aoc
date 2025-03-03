package com.keithalcock.aoc.year2021.day1

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 7
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day1/test.txt")

    expectedResult should be (actualResult)
  }
}

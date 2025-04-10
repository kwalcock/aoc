package com.keithalcock.aoc.year2021.day21

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 739785
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day21/test.txt")

    actualResult should be (expectedResult)
  }
}

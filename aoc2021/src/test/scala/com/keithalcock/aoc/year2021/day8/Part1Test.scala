package com.keithalcock.aoc.year2021.day8

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 26
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day8/test.txt")

    actualResult should be (expectedResult)
  }
}

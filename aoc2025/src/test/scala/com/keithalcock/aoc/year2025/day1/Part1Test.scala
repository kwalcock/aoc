package com.keithalcock.aoc.year2025.day1

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Day1.Part1"

  it should "behave" in {
    val expectedResult = 7
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day1/test.txt")

    actualResult should be (expectedResult)
  }
}

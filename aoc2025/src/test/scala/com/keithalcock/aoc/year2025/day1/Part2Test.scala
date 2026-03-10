package com.keithalcock.aoc.year2025.day1

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Day1.Part2"

  it should "behave" in {
    val expectedResult = 6
    val actualResult = Part2.run(50, "com/keithalcock/aoc/year2025/day1/test.txt")

    actualResult should be (expectedResult)
  }
}

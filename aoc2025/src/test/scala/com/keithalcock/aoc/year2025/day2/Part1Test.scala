package com.keithalcock.aoc.year2025.day2

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Day2.Part1"

  it should "behave" in {
    val expectedResult = 1227775554
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day2/test.txt")

    actualResult should be (expectedResult)
  }
}

package com.keithalcock.aoc.year2025.day8

import com.keithalcock.aoc.Test

class Part1Test extends Test:

  behavior of "Day8.Part1"

  it should "behave" in:
    val expectedResult = 40
    val actualResult = Part1.run(10, "com/keithalcock/aoc/year2025/day8/test.txt")

    actualResult should be (expectedResult)

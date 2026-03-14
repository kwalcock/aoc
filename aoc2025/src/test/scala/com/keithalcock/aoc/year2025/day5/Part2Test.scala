package com.keithalcock.aoc.year2025.day5

import com.keithalcock.aoc.Test

class Part2Test extends Test:

  behavior of "Day5.Part2"

  it should "behave" in:
    val expectedResult = 14
    val actualResult = Part2.run("com/keithalcock/aoc/year2025/day5/test.txt")

    actualResult should be (expectedResult)

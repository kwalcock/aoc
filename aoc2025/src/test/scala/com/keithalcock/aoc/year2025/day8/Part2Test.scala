package com.keithalcock.aoc.year2025.day8

import com.keithalcock.aoc.Test

class Part2Test extends Test:

  behavior of "Day8.Part2"

  it should "behave" in:
    val expectedResult = 25272
    val actualResult = Part2.run("com/keithalcock/aoc/year2025/day8/test.txt")

    actualResult should be (expectedResult)

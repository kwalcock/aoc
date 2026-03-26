package com.keithalcock.aoc.year2025.day9

import com.keithalcock.aoc.Test

class Part1Test extends Test:

  behavior of "Day9.Part1"

  it should "behave" in:
    val expectedResult = 50
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day9/test.txt")

    actualResult should be (expectedResult)

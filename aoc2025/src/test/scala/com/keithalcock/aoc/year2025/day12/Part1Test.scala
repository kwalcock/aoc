package com.keithalcock.aoc.year2025.day12

import com.keithalcock.aoc.Test

class Part1Test extends Test:

  behavior of "Day12.Part1"

  it should "behave" in:
    val expectedResult = 2
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day12/test.txt")

    actualResult should be (expectedResult)

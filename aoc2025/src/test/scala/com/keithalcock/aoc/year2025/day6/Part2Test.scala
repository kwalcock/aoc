package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.Test

class Part2Test extends Test:

  behavior of "Day6.Part2"

  it should "behave" in:
    val expectedResult = 3263827L
    val actualResult = Part2.run("com/keithalcock/aoc/year2025/day6/test.txt")

    actualResult should be (expectedResult)

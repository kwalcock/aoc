package com.keithalcock.aoc.year2025.day4

import com.keithalcock.aoc.Test

class Part2Test extends Test:

  behavior of "Day4.Part2"

  it should "behave" in:
    val expectedResult = 43
    val actualResult = Part2.run("com/keithalcock/aoc/year2025/day4/test.txt")

    actualResult should be (expectedResult)

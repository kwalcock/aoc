package com.keithalcock.aoc.year2025.day3

import com.keithalcock.aoc.Test

class Part1Test extends Test:

  behavior of "Day3.Part1"

  it should "behave" in:
    val expectedResult = 357
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day3/test.txt")

    actualResult should be (expectedResult)

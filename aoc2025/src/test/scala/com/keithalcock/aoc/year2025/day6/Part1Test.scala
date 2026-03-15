package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.Test

class Part1Test extends Test:

  behavior of "Day6.Part1"

  it should "behave" in:
    val expectedResult = 4277556L
    val actualResult = Part1.run("com/keithalcock/aoc/year2025/day6/test.txt")

    actualResult should be (expectedResult)

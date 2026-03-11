package com.keithalcock.aoc.year2025.day2

import com.keithalcock.aoc.Test

class Part2Test extends Test:

  behavior of "Day2.Part2"

  it should "behave" in:
    val expectedResult = 4174379265L
    val actualResult = Part2.run("com/keithalcock/aoc/year2025/day2/test.txt")

    actualResult should be (expectedResult)

package com.keithalcock.aoc.year2025.day1

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Rotation"

  it should "behave" in {
    val size = 100
    val l = 'L'
    val r = 'R'

    new Rotation(size, l, 0).value should be (0)
    new Rotation(size, l, 99).value should be (99)
    new Rotation(size, l, 101).value should be (1)

    new Rotation(size, r, 0).value should be (0)
    new Rotation(size, r, 99).value should be (1)
    new Rotation(size, r, 101).value should be (99)
  }

  behavior of "Day1.Part1"

  it should "behave" in {
    val expectedResult = 3
    val actualResult = Part1.run(50, "com/keithalcock/aoc/year2025/day1/test.txt")

    actualResult should be (expectedResult)
  }
}

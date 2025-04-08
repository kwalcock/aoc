package com.keithalcock.aoc.year2021.day19

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 3621
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day19/test.txt")

    actualResult should be (expectedResult)
  }
}

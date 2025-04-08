package com.keithalcock.aoc.year2021.day20

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 3351
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day20/test.txt")

    actualResult should be (expectedResult)
  }
}

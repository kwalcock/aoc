package com.keithalcock.aoc.year2021.day6

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "behave for 256days" in {
    val days = 256
    val expectedResult = 26984457539L
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day6/test.txt", days)

    actualResult should be (expectedResult)
  }
}

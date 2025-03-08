package com.keithalcock.aoc.year2021.day6

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave for 18 days" in {
    val days = 18
    val expectedResult = 26
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day6/test.txt", days)

    expectedResult should be (actualResult)
  }

  it should "behave for 26 days" in {
    val days = 80
    val expectedResult = 5934
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day6/test.txt", days)

    expectedResult should be (actualResult)
  }
}

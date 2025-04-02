package com.keithalcock.aoc.year2021.day16

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave for test1" in {
    val expectedResult = 16
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day16/test1.txt")

    expectedResult should be (actualResult)
  }

  it should "behave for test2" in {
    val expectedResult = 12
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day16/test2.txt")

    expectedResult should be (actualResult)
  }

  it should "behave for test3" in {
    val expectedResult = 23
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day16/test3.txt")

    expectedResult should be (actualResult)
  }

  it should "behave for test4" in {
    val expectedResult = 31
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day16/test4.txt")

    expectedResult should be (actualResult)
  }
}

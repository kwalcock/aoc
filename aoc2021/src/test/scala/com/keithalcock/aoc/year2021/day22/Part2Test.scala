package com.keithalcock.aoc.year2021.day22

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 474140
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day22/test2.txt")

    actualResult should be (expectedResult)
  }

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 2758514936282235L
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day22/test2.txt")

    actualResult should be (expectedResult)
  }
}

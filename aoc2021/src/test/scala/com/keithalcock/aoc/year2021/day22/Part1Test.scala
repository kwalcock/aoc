package com.keithalcock.aoc.year2021.day22

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 590784
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day22/test1.txt")

    actualResult should be (expectedResult)
  }
}

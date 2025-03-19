package com.keithalcock.aoc.year2021.day12

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part2"

  it should "pass the second test" in {
    val expectedResult = 103
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day12/test2.txt")

    actualResult should be (expectedResult)
  }

  it should "pass the third test" in {
    val expectedResult = 3509
    val actualResult = Part2.run("com/keithalcock/aoc/year2021/day12/test3.txt")

    actualResult should be (expectedResult)
  }
}

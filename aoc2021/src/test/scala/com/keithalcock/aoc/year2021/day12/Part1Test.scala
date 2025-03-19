package com.keithalcock.aoc.year2021.day12

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "pass the first test" in {
    val expectedResult = 10
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day12/test1.txt")

    expectedResult should be (actualResult)
  }

  it should "pass the second test" in {
    val expectedResult = 19
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day12/test2.txt")

    expectedResult should be (actualResult)
  }

  it should "pass the third test" in {
    val expectedResult = 226
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day12/test3.txt")

    expectedResult should be (actualResult)
  }
}

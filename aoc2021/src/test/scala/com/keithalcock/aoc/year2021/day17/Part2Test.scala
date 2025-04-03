package com.keithalcock.aoc.year2021.day17

import com.keithalcock.aoc.Test

class Part2Test extends Test {
  val targetX = Range.inclusive(20, 30)
  val targetY = Range.inclusive(-10, -5)

  behavior of "Part2"

  it should "behave" in {
    val expectedResult = 112
    val actualResult = Part2.run(targetX, targetY)

    expectedResult should be (actualResult)
  }
}

package com.keithalcock.aoc.year2021.day17

import com.keithalcock.aoc.Test

class Part1Test extends Test {
  val targetX = Range.inclusive(20, 30)
  val targetY = Range.inclusive(-10, -5)

  behavior of "Part1"

  it should "behave for test1" in {
    val expectedResult = Some(3)
    val actualResult = Part1.run(targetX, targetY, 7, 2)

    expectedResult should be (actualResult)
  }

  it should "behave for test2" in {
    val expectedResult = Some(6)
    val actualResult = Part1.run(targetX, targetY, 6, 3)

    expectedResult should be (actualResult)
  }

  it should "behave for test3" in {
    val expectedResult = Some(0)
    val actualResult = Part1.run(targetX, targetY, 9, 0)

    expectedResult should be (actualResult)
  }

  it should "behave for test4" in {
    val expectedResult = None
    val actualResult = Part1.run(targetX, targetY, 17, -4)

    expectedResult should be (actualResult)
  }

  it should "behave for test5" in {
    val expectedResult = Some(45)
    val actualResult = Part1.run(targetX, targetY, 6, 9)

    expectedResult should be (actualResult)
  }
}

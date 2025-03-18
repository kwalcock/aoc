package com.keithalcock.aoc.year2021.day11

import com.keithalcock.aoc.Test

class Part1Test extends Test {
  val days = Seq(
      1,  2,  3,  4,  5,  6,  7,  8,  9,
     10, 20, 30, 40, 50, 60, 70, 80, 90,
    100
  )

  behavior of "Part1"

  it should "behave" in {
    val expectedResult = 1656
    val actualResult = Part1.run("com/keithalcock/aoc/year2021/day11/test.txt", 100)

    expectedResult should be (actualResult)
  }

  it should "get the correct intermediate values" in {
    val grid = Part1.mkGrid("com/keithalcock/aoc/year2021/day11/test.txt")

    1.to(days.last).foreach { day =>
      grid.step
      if (days.contains(day)) {
        val expectedResult = Part1.mkGrid(s"com/keithalcock/aoc/year2021/day11/test$day.txt")
        val actualResult = grid

        actualResult should be (expectedResult)
      }
    }
  }
}

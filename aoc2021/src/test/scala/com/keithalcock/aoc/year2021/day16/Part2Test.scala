package com.keithalcock.aoc.year2021.day16

import com.keithalcock.aoc.Test

class Part2Test extends Test {

  behavior of "Part1"

  it should "behave for test1" in {
    val expectedResult = 3
    val actualResult = Part2.runString("C200B40A82")

    expectedResult should be (actualResult)
  }

  it should "behave for test2" in {
    val expectedResult = 54
    val actualResult = Part2.runString("04005AC33890")

    expectedResult should be (actualResult)
  }

  it should "behave for test3" in {
    val expectedResult = 7
    val actualResult = Part2.runString("880086C3E88112")

    expectedResult should be (actualResult)
  }

  it should "behave for test4" in {
    val expectedResult = 9
    val actualResult = Part2.runString("CE00C43D881120")

    expectedResult should be (actualResult)
  }

  it should "behave for test5" in {
    val expectedResult = 1
    val actualResult = Part2.runString("D8005AC2A8F0")

    expectedResult should be (actualResult)
  }

  it should "behave for test6" in {
    val expectedResult = 0
    val actualResult = Part2.runString("F600BC2D8F")

    expectedResult should be (actualResult)
  }

  it should "behave for test7" in {
    val expectedResult = 0
    val actualResult = Part2.runString("9C005AC2F8F0")

    expectedResult should be (actualResult)
  }

  it should "behave for test8" in {
    val expectedResult = 1
    val actualResult = Part2.runString("9C0141080250320F1802104A08")

    expectedResult should be (actualResult)
  }
}

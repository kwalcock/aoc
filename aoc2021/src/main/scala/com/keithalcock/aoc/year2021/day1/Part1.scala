package com.keithalcock.aoc.year2021.day1

import scala.io.Source

object Part1 extends App {
  val depths = Source
      .fromResource("com/keithalcock/aoc/year2021/day1/input.txt")
      .getLines
      .map(_.toInt)
  val increasingCount = depths
      .sliding(2)
      .count { case Seq(left, right) => left < right }

  println(increasingCount)
}

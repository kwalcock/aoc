package com.keithalcock.aoc.year2021.day1

import scala.io.Source

object Part2 extends App {
  val width = 3
  val depths = Source
      .fromResource("com/keithalcock/aoc/year2021/day1/input.txt")
      .getLines
      .map(_.toInt)
  val increasingCount = depths
      .sliding(width + 1)
      .count { seq =>
        val prev = seq.head
        val next = seq.last

        prev < next
      }

  println(increasingCount)
}

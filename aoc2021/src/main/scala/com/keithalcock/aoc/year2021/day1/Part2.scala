package com.keithalcock.aoc.year2021.day1

import scala.io.Source

object Part2 {

  def run(resourceName: String): Int = {
    val width = 3
    val depths = Source
        .fromResource(resourceName)
        .getLines
        .map(_.toInt)
    val increasingCount = depths
        .sliding(width + 1)
        .count { seq =>
          val prev = seq.head
          val next = seq.last

          prev < next
        }

    increasingCount
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day1/input.txt")

  println(result)
}

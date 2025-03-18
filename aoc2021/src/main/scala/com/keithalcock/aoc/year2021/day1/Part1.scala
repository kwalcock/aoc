package com.keithalcock.aoc.year2021.day1

import scala.io.Source

object Part1 {

  def run(resourceName: String): Int = {
    val depths = Source
        .fromResource(resourceName)
        .getLines
        .map(_.toInt)
    val increasingCount = depths
        .sliding(2)
        .count { case Seq(prev, next) => prev < next }

    increasingCount
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day1/input.txt")

  println(result)
}

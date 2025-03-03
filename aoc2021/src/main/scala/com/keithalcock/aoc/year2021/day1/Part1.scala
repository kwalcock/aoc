package com.keithalcock.aoc.year2021.day1

import scala.io.Source

object Part1 extends App {

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

  val result = run("com/keithalcock/aoc/year2021/day1/input.txt")

  println(result)
}

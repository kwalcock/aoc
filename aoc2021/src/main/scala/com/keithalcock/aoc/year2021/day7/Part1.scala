package com.keithalcock.aoc.year2021.day7

import scala.io.Source

object Part1 {

  def mkPositions(line: String): Array[Int] = line.split(',').map(_.toInt)

  def calcFuel(positions: Array[Int]): Int = {
    val min = positions.min
    val max = positions.max
    val fuels = min.to(max).map { target =>
      positions.map { position =>
        math.abs(target - position)
      }.sum
    }

    fuels.min
  }

  def run(resourceName: String): Int = {
    val line = Source
        .fromResource(resourceName)
        .getLines
        .toList
        .head
    val positions = mkPositions(line)
    val overlap = calcFuel(positions)

    overlap
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day7/input.txt")

  println(result)
}

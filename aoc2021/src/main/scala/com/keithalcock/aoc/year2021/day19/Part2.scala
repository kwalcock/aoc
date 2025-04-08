package com.keithalcock.aoc.year2021.day19

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Int] {

  def run(lines: Iterator[String]): Int = {
    val orientedScanners = Part1.mkOrientedScanners(lines)
    val points = orientedScanners.map(_.point)
    val distances = points.flatMap { outerPoint =>
      points.map { innerPoint =>
          outerPoint.manhattanDistance(innerPoint)
      }
    }
    val maxDistance = distances.max

    maxDistance
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day19/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2021.day8

import scala.io.Source

object Part1 {
  val uniqueLengths = Set(2, 3, 4, 7)

  def count1478(lines: Seq[String]): Int = {
    val total = lines.foldLeft(0) { (subtotal, line) =>
      val lengths = line.split('|').last.split(' ').filterNot(_.isEmpty).map(_.length)
      val count = lengths.count { length => uniqueLengths(length) }

      subtotal + count
    }

    total
  }

  def run(resourceName: String): Int = {
    val lines = Source
        .fromResource(resourceName)
        .getLines
        .toList
    val count = count1478(lines)

    count
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day8/input.txt")

  println(result)
}

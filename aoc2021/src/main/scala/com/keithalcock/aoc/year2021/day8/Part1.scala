package com.keithalcock.aoc.year2021.day8

import com.keithalcock.aoc.year2021.Aoc

import scala.io.Source

object Part1 extends Aoc[Int] {
  val uniqueLengths = Set(2, 3, 4, 7)

  def count1478(lines: Seq[String]): Int = {
    val total = lines.foldLeft(0) { (subtotal, line) =>
      val lengths = line.split('|').last.split(' ').filterNot(_.isEmpty).map(_.length)
      val count = lengths.count { length => uniqueLengths(length) }

      subtotal + count
    }

    total
  }

  def run(lines: Iterator[String]): Int = {
    val linesList = lines.toList
    val count = count1478(linesList)

    count
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day8/input.txt")

  println(result)
}

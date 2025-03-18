package com.keithalcock.aoc.year2021.day6

import scala.io.Source
import scala.util.Using

class Timers(values: Array[Int]) {
  val newArray = Array(6, 8)

  def length: Int = values.length

  def next: Timers = {
    val newValues = values.flatMap { value =>
      if (value == 0) newArray
      else Array(value - 1)
    }

    new Timers(newValues)
  }
}

object Part1 {

  def mkTimers(line: String): Timers = {
    val values = line.split(',').map(_.toInt)

    new Timers(values)
  }

  def run(lines: Iterator[String], days: Int): Int = {
    val line = lines
        .toList
        .head
    val timers = mkTimers(line)
    val newTimers = 1.to(days).foldLeft(timers) { case (timers, _) =>
      timers.next
    }

    newTimers.length
  }

  def run(resourceName: String, days: Int): Int = {
    Using.resource(Source.fromResource(resourceName)) { source =>
      run(source.getLines, days)
    }
  }
}

object Part1App extends App {
  val days = 80
  val result = Part1.run("com/keithalcock/aoc/year2021/day6/input.txt", days)

  println(result)
}

package com.keithalcock.aoc.year2021.day6

import scala.io.Source

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

object Part1 extends App {

  def mkTimers(line: String): Timers = {
    val values = line.split(',').map(_.toInt)

    new Timers(values)
  }

  def run(resourceName: String, days: Int): Int = {
    val line = Source
        .fromResource(resourceName)
        .getLines
        .toList
        .head
    val timers = mkTimers(line)
    val newTimers = 1.to(days).foldLeft(timers) { case (timers, _) =>
      timers.next
    }

    newTimers.length
  }

  val days = 80
  val result = run("com/keithalcock/aoc/year2021/day6/input.txt", days)

  println(result)
}

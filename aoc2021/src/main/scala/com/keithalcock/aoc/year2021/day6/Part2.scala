package com.keithalcock.aoc.year2021.day6

import scala.io.Source
import scala.util.Using

class Counters(values: Array[Long]) {

  def length: Long = values.sum

  def next: Counters = {
    val newValues = values.indices.map { index =>
      if (index == 6) values(0) + values(7) // These all spawn (go from 0 to 6) or go from 7 to 6.
      else if (index == 8) values(0) // These all spawn (go from 0 to 8).
      else values(index + 1) // Timers of everything else just go from index + 1 to index
    }.toArray

    new Counters(newValues)
  }
}

object Part2 {

  def mkCounters(line: String): Counters = {
    val maxDays = 8
    val values = line.split(',').map(_.toInt)
    val counts = Array.tabulate(maxDays + 1) { index =>
      values.count(_ == index).toLong
    }

    new Counters(counts)
  }

  def run(lines: Iterator[String], days: Int): Long = {
    val line = lines
        .toList
        .head
    val counters = mkCounters(line)
    val newCounters = 1.to(days).foldLeft(counters) { case (counters, _) =>
      val newCounters = counters.next

      newCounters
    }

    newCounters.length
  }

  def run(resourceName: String, days: Int): Long = {
    Using.resource(Source.fromResource(resourceName)) { source =>
      run(source.getLines, days)
    }
  }
}

object Part2App extends App {
  val days = 256
  val result = Part2.run("com/keithalcock/aoc/year2021/day6/input.txt", days)

  println(result)
}

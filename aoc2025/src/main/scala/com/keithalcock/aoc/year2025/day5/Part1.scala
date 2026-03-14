package com.keithalcock.aoc.year2025.day5

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Int]:

  // NumericRange start.to(end) can't have more than Int.MaxValue elements.
  case class LongRange(start: Long, end: Long):

    def contains(value: Long): Boolean =
      start <= value && value <= end

  def run(lines: Iterator[String]): Int =
    val ranges = lines
      .takeWhile(_.nonEmpty)
      .collect:
        case s"${start}-${end}" => LongRange(start.toLong, end.toLong)
      .toSeq
    val count = lines
      .map(_.toLong)
      .count:
        value => ranges.exists(_.contains(value))

    count

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day5/input.txt")

  println(result)

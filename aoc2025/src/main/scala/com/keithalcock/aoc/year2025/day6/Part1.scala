package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Long]:

  def run(lines: Iterator[String]): Long =
    val problems = lines
      .map(_.trim.split("\\s+"))
      .toIndexedSeq
    val widthRange = problems.head.indices
    val lengthRange = problems.indices
    val results = widthRange.map: widthIndex =>
      val strings = lengthRange.map: lengthIndex =>
        problems(lengthIndex)(widthIndex)
      val values = strings.init.map(_.toLong)
      val operation = strings.last
      val result = operation match
        case "*" => values.reduce(_ * _)
        case "+" => values.reduce(_ + _)

      result
    val result = results.sum

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day6/input.txt")

  println(result)

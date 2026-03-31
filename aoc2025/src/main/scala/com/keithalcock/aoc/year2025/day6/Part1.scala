package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part1 extends Aoc[Long]:

  def run(lines: Iterator[String]): Long =
    val problems = lines
      .map(_.trim.split(raw"\s+"))
      .toIndexedSeq
    val widthRange = problems.head.indices
    val lengthRange = problems.indices
    val results = widthRange.map: widthIndex =>
      val strings = lengthRange.map: lengthIndex =>
        problems(lengthIndex)(widthIndex)
      val values = strings.init.map(_.toLong)
      val operation = strings.last
      val result = operation match
        case "*" => values.product
        case "+" => values.sum

      result
    val result = results.sum

    result

  def run2(lines: Iterator[String]): Long =

    def toStrings(line: String): Array[String] = line.trim.split(raw"\s+")

    @tailrec
    def loop(lines: Iterator[String], pairs: Array[(Long, Long)]): Array[Long] =
      val line = lines.next
      val strings = toStrings(line)

      if !lines.hasNext then
        val monos = pairs.zip(strings).map:
          case (pair, "+") => pair._1
          case (pair, "*") => pair._2
          case (_, _) => ???

        monos
      else
        val values = strings.map(_.toLong)
        val newPairs = pairs.zip(values).map:
          case (pair, value) =>
            (pair._1 + value, pair._2 * value) // Perform both calculations.

        loop(lines, newPairs)

    val headPairs = toStrings(lines.next).map: value =>
      (value.toLong, value.toLong)
    val results = loop(lines, headPairs)
    val result = results.sum

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day6/input.txt")

  println(result)

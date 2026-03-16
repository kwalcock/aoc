package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.year2025.Aoc

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
    val headPairs = lines.next.trim.split(raw"\s+").map: value =>
      (value.toLong, value.toLong)

    val pairs = lines.foldLeft(headPairs): (pairs, line) =>
      val strings = line.trim.split(raw"\s+")

      strings.head match
        case "+" | "*" =>
          pairs.zip(strings).map:
            case (pair, string) =>
              string match
                case "+" => (pair._1, 0) // Keep just the sum.
                case "*" => (0, pair._2) // Keep just the product.
        case _ =>
          pairs.zip(strings).map:
            case (pair, string) =>
              val value = string.toLong

              (pair._1 + value, pair._2 * value) // Perform both calculations.

    val results = pairs.map: pair =>
      pair._1 + pair._2
    val result = results.sum

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day6/input.txt")

  println(result)

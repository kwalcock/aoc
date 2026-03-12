package com.keithalcock.aoc.year2025.day3

import com.keithalcock.aoc.year2025.Aoc

object Part2 extends Aoc[Long]:

  def joltage(string: String): Long =
    val length = string.length
    val (left, total) = 1.to(12).foldLeft((-1, 0L)):
      case ((prev, total), index) =>
        // Start to the right of previous find.
        // Leave enough places on the right for the remaining digits.
        val start = prev + 1
        val substring = string.view.drop(start).dropRight(12 - index)
        val digit = substring.max
        val leftIndex = start + substring.indexOf(digit)
        val newTotal = total * 10 + digit.asDigit

        (leftIndex, newTotal)

    total

  def run(lines: Iterator[String]): Long =
    val scores = lines
      .map(joltage)
      .toSeq

    val result = scores.sum
    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day3/input.txt")

  println(result)

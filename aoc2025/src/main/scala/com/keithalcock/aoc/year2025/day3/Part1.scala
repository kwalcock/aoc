package com.keithalcock.aoc.year2025.day3

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Int]:

  def joltage(string: String): Int =
    val left = string.dropRight(1).max
    val leftIndex = string.indexOf(left)
    val right = string.drop(leftIndex + 1).max

    left.toString.toInt * 10 + right.toString.toInt

  def run(lines: Iterator[String]): Int =
    val scores = lines
      .map(joltage)
      .toSeq

    val result = scores.sum
    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day3/input.txt")

  println(result)

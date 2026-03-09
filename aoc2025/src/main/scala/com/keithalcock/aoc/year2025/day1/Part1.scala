package com.keithalcock.aoc.year2025.day1

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Int]:

  def run(lines: Iterator[String]): Int =
    val depths = lines
        .map(_.toInt)
    val increasingCount = depths
        .sliding(2)
        .count { case Seq(prev, next) => prev < next }

    increasingCount

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day1/input.txt")

  println(result)


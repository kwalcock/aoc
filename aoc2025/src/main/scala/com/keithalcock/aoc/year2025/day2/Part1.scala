package com.keithalcock.aoc.year2025.day2

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Long]:

  def isInvalid(valid: Long): Boolean =
    val string = valid.toString

    if string.length % 2 != 0 then
      false
    else
      val half = string.length / 2

      0.until(half).forall: index =>
        string.charAt(index) == string.charAt(index + half)

  def run(lines: Iterator[String]): Long =
    val invalidIDs = lines
      .flatMap(_.isplit(','))
      .map(_.isplit('-'))
      .map(iArray => iArray.head.toLong.to(iArray.last.toLong))
      .flatMap(_.filter(isInvalid))

    val result = invalidIDs.sum
    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day2/input.txt")

  println(result)

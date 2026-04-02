package com.keithalcock.aoc.year2025.day2

import com.keithalcock.aoc.year2025.Aoc

object Part2 extends Aoc[Long]:

  def isInvalid(valid: Long): Boolean =
    val string = valid.toString

    2.to(string.length).exists: partCount =>
      if string.length % partCount == 0 then
        val partLength = string.length / partCount

        0.until(partLength).forall: offsetIndex =>
          1.until(partCount).forall: partIndex =>
            string.charAt(offsetIndex) == string.charAt(offsetIndex + partIndex * partLength)
      else
        false

  def run(lines: Iterator[String]): Long =
    val invalidIDs = lines
      .flatMap(_.isplit(','))
      .map(_.isplit('-'))
      .map(iArray => iArray.head.toLong.to(iArray.last.toLong))
      .flatMap(_.filter(isInvalid))

    val result = invalidIDs.sum
    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day2/input.txt")

  println(result)

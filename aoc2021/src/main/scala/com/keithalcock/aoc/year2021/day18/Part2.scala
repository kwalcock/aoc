package com.keithalcock.aoc.year2021.day18

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Int] {

  def run(strings: Seq[String]): Int = {
    val magnitudes = strings.flatMap { leftString =>
      strings.flatMap { rightString =>
        if (leftString != rightString)
          Some(DoubleSnailfish.add(DoubleSnailfish(leftString), DoubleSnailfish(rightString)).magnitude)
        else None
      }
    }
    val max = magnitudes.max

    max
  }

  def run(lines: Iterator[String]): Int = {
    run(lines.toArray)
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day18/input.txt")

  println(result)
}

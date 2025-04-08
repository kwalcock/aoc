package com.keithalcock.aoc.year2021.day20

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Int] {

  def run(lines: Iterator[String]): Int = {
    val rule = Part1.mkRule(lines)
    val scan = Part1.mkScan(lines)
    val scan50 = Range(0, 50).foldLeft(scan) { case (scan, _) =>
      scan.apply(rule)
    }
    val count = scan50.count

    count
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day20/input.txt")

  println(result)
}

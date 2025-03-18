package com.keithalcock.aoc.year2021.day11

import com.keithalcock.aoc.year2021.Aoc

import scala.io.Source

object Part2 extends Aoc[Int] {

  def run(lines: Iterator[String]): Int = {
    val grid = Part1.mkGrid(lines)
    val size = grid.dim * grid.dim

    @annotation.tailrec
    def loop(index: Int): Int = {
      val count = grid.step

      if (count == size) index + 1
      else loop(index + 1)
    }

    loop(0)
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day11/input.txt")

  println(result)
}

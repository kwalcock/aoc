package com.keithalcock.aoc.year2021.day11

object Part2 {

  def run(resourceName: String): Int = {
    val grid = Part1.mkGrid(resourceName)
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

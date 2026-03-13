package com.keithalcock.aoc.year2025.day4

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part2 extends Aoc[Int]:
  val paper = '@'
  val nonPaper = '.'
  val inventory = Set(paper, nonPaper)

  class Grid(val lines: Seq[String]):
    val dim1 = lines.head.length
    val dim2 = lines.length

    require(lines.forall(_.length == dim1))
    require(lines.forall(_.forall(inventory)))

    val range1 = lines.head.indices
    val range2 = lines.indices
    val rollCount = countIf: (index1: Int, index2: Int) =>
      atOpt(index1, index2).contains(paper)

    def atOpt(index1: Int, index2: Int): Option[Char] =
      if range1.contains(index1) && range2.contains(index2) then
        Some(lines(index2)(index1))
      else
        None

    def countIf(f: (index1: Int, index2: Int) => Boolean): Int =
      var count = 0

      range2.foreach: index2 =>
        range1.foreach: index1 =>
          if f(index1, index2) then
            count += 1
      count

    def countNeighborsIf(index1: Int, index2: Int, f: (Char) => Boolean): Int =
      var count = 0

      -1.to(1).foreach: delta2 =>
        -1.to(1).foreach: delta1 =>
          if delta1 != 0 || delta2 != 0 then
            val itemOpt = atOpt(index1 + delta1, index2 + delta2)

            if itemOpt.exists(f) then
              count += 1
      count

    def removeRolls(): Grid =
      val stringBuffers = Seq.fill(dim2)(new StringBuffer(dim1))

      range2.foreach: index2 =>
        val stringBuffer = stringBuffers(index2)

        range1.foreach: index1 =>
          val cell = atOpt(index1, index2).get

          if cell == nonPaper then
            stringBuffer.append(cell)
          else if countNeighborsIf(index1, index2, _ == paper) < 4 then
            stringBuffer.append(nonPaper)
          else
            stringBuffer.append(cell)

      val strings = stringBuffers.map(_.toString)
      new Grid(strings)

  def run(lines: Iterator[String]): Int =
    val grid = Grid(lines.toSeq)

    @tailrec
    def loop(grid: Grid): Grid =
      val newGrid = grid.removeRolls()

      if newGrid.rollCount == grid.rollCount then
        newGrid
      else
        loop(newGrid)

    val newGrid = loop(grid)

    grid.rollCount - newGrid.rollCount

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day4/input.txt")

  println(result)

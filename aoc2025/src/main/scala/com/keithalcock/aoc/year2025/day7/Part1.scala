package com.keithalcock.aoc.year2025.day7

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part1 extends Aoc[Int]:

  def run(lines: Iterator[String]): Int =
    val bufferedLines = lines.buffered
    val width = bufferedLines.head.length

    @tailrec
    def loop(topLine: String, count: Int): Int =
      if !lines.hasNext then count
      else
        val middleLine =
          val line = bufferedLines.next
          require(line.forall("S^.".contains))
          require(line.length == width)
          line
        val bottomLine =
          val line = bufferedLines.next
          require(line.forall(".".contains))
          require(line.length == width)
          line
        val newBottomLineChars = bottomLine.indices.map: index =>
          if middleLine(index) == 'S' then '|' // It starts.
          else if middleLine(index) == '^' then '.'  // It is blocked.
          else // middleLine(index) == '.'
            if (middleLine.lift(index - 1).contains('^') && topLine(index - 1) == '|') ||
                (middleLine.lift(index + 1).contains('^') && topLine(index + 1) == '|') then
              '|' // It split.
            else
              topLine(index) // It fell through.
        val newBottomLine = String(newBottomLineChars.toArray)
        val increment = topLine.zip(middleLine).count: (topChar, middleChar) =>
          topChar == '|' && middleChar == '^'

        loop(String(newBottomLine), count + increment)

    val count = loop("." * width, 0)
    count

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day7/input.txt")

  println(result)

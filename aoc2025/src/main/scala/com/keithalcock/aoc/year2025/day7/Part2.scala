package com.keithalcock.aoc.year2025.day7

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part2 extends Aoc[Long]:

  def run(lines: Iterator[String]): Long =
    val bufferedLines = lines.buffered
    val width = bufferedLines.head.length

    @tailrec
    def loop(topLine: String, timelines: Seq[Long]): Long =
      if !lines.hasNext then timelines.sum
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
        val newBottomLineCharsAndTimeline = bottomLine.indices.map: index =>
          if middleLine(index) == 'S' then
            ('|', 1L) // It starts and there is only one way a particle is here.
          else if middleLine(index) == '^' then
            ('.', 0L)  // It is blocked and there is no way there is a particle here.
          else // middleLine(index) == '.'
            val fromLeft = middleLine.lift(index - 1).contains('^') && topLine(index - 1) == '|'
            val fromRight = middleLine.lift(index + 1).contains('^') && topLine(index + 1) == '|'
            val fromTop = middleLine(index) == '.' && topLine(index) == '|'
            val fromLeftCount = if fromLeft then timelines(index - 1) else 0
            val fromRightCount = if fromRight then timelines(index + 1) else 0
            val fromTopCount = if fromTop then timelines(index) else 0
            val count = fromLeftCount + fromRightCount + fromTopCount

            (if count > 0 then'|' else '.', count) // It split, so combine the timelines.

        val (newBottomLineChars, newTimelines) = newBottomLineCharsAndTimeline.unzip
        val newBottomLine = String(newBottomLineChars.toArray)
        val increment = topLine.zip(middleLine).count: (topChar, middleChar) =>
          topChar == '|' && middleChar == '^'

        loop(newBottomLine, newTimelines)

    val count = loop("." * width, Seq.fill(width)(0L))

    count

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day7/input.txt")

  println(result)

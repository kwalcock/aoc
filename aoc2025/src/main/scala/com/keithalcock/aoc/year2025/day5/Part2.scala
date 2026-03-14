package com.keithalcock.aoc.year2025.day5

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part2 extends Aoc[Long]:

  // NumericRange start.to(end) can't have more than Int.MaxValue elements.
  case class LongRange(start: Long, end: Long):

    def contains(value: Long): Boolean =
      start <= value && value <= end

    def length: Long =
      if start <= end then
        end - start + 1
      else
        0

    def compare(value: Long): Int =
      if value < start then -1
      else if end < value then 1
      else 0

    def intersects(range: LongRange): Boolean =
      val comparedStart = compare(range.start)
      val comparedEnd = compare(range.end)

      comparedStart == 0 || comparedEnd == 0 || comparedStart != comparedEnd

    def disjoin(range: LongRange): Seq[LongRange] =
      val comparedStart = compare(range.start)
      val comparedEnd = compare(range.end)

      (comparedStart, comparedEnd) match
        case (-1, -1) => Seq(this)
        case (-1, 0) => Seq(LongRange(range.end + 1, end))
        case (-1, 1) => Seq.empty

        case (0, 0) => Seq(LongRange(start, range.start - 1), LongRange(range.end + 1, end))
        case (0, 1) => Seq(LongRange(start, range.start - 1))

        case (1, 1) => Seq(this)
  end LongRange

  def disjoin(ranges: Seq[LongRange]): Seq[LongRange] =

    @tailrec
    def loop(unfinisheds: Seq[LongRange], finisheds: Seq[LongRange]): Seq[LongRange] =
      if unfinisheds.isEmpty then
        finisheds
      else
        val head = unfinisheds.head
        val tail = unfinisheds.tail

        if head.length == 0 then
          loop(tail, finisheds)
        else
          val intersectOpt = tail.find(head.intersects)

          if intersectOpt.isEmpty then
            loop(tail, finisheds :+ head)
          else
            val disjoineds = head.disjoin(intersectOpt.get)

            loop(disjoineds ++ tail, finisheds)
    end loop

    loop(ranges, Seq.empty)
  end disjoin

  def run(lines: Iterator[String]): Long =
    val ranges = lines
      .takeWhile(_.nonEmpty)
      .collect:
        case s"${start}-${end}" => LongRange(start.toLong, end.toLong)
      .toSeq
    val disjoinedRanges = disjoin(ranges)
    val count = disjoinedRanges.map(_.length).sum

    count

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day5/input.txt")

  println(result)

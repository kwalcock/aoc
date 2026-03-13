package com.keithalcock.aoc.year2025.day1

import scala.io.Source
import scala.util.Using

object Part2:
  val size = 100

  extension (value: Int)
    def %+(base: Int): Int = math.floorMod(value, base)

  case class Rotation(value: Int):

    def this(direction: Char, amount: Int) =
      this {
        direction match
          case 'L' => -amount
          case 'R' => amount
          case _ => ???
      }

    def this(string: String) =
      this(string.head, string.tail.toInt)

    def fullCount = value.abs / size

    def restCount = value % size

  def run(start: Int, resourceName: String): Int =
    Using.resource(Source.fromResource(resourceName)): source =>
      run(start, source)

  def run(start: Int, source: Source): Int =
    run(start, source.getLines)

  def run(start: Int, lines: Iterator[String]): Int =
    val count = lines
        .map: line =>
          new Rotation(line)
        .scanLeft((start, 0)): (currentAndCount, rotation: Rotation) =>
          val (current, _) = currentAndCount
          val value = rotation.value
          val (newCurrent, newCount) =
            if value > 0 then
              val tmpCurrent = current + rotation.restCount
              // The current > 0 isn't really used here.
              val tmpCount = rotation.fullCount + (if rotation.restCount != 0 && current > 0 && tmpCurrent >= size then 1 else 0)

              (tmpCurrent, tmpCount)
            else if value < 0 then
              val tmpCurrent = current + rotation.restCount
              // The current > 0 is so that it passes 0 if not already on 0.
              val tmpCount = rotation.fullCount + (if rotation.restCount != 0 && current > 0 && tmpCurrent <= 0 then 1 else 0)

              (tmpCurrent, tmpCount)
            else
              (current, 0) // Don't count if not moved, especially if it is already at 0.

          (newCurrent %+ size, newCount)
        .map(_._2) // Isolate the counts.
        .sum

    count

object Part2App extends App:
  val result = Part2.run(50, "com/keithalcock/aoc/year2025/day1/input.txt")

  println(result)

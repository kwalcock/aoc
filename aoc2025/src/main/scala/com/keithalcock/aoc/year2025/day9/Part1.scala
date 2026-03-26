package com.keithalcock.aoc.year2025.day9

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Long]:

  case class Coordinate(x: Int, y: Int):

    def area(other: Coordinate): Long =
      (x - other.x + 1).abs.toLong * (y - other.y + 1).abs.toLong

  def run(lines: Iterator[String]): Long =
    val coordinates = lines
      .map:
        case s"$x,$y" => Coordinate(x.toInt, y.toInt)
      .toSeq
      .combinations(2)
      .maxBy:
        case Seq(coordinate1, coordinate2) =>
          coordinate1.area(coordinate2)
    val result = coordinates.head.area(coordinates.last)

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day9/input.txt")

  println(result)

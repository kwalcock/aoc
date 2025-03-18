package com.keithalcock.aoc.year2021.day5

import scala.io.Source

class DiagonalLineSegment(x1: Int, y1: Int, x2: Int, y2: Int) extends LineSegment(x1, y1, x2, y2) {

  def draw(array: Array[Array[Int]]): Unit = {
    val dx = math.signum(x2 - x1)
    val dy = math.signum(y2 - y1)
    val count = math.abs(x1 - x2)

    0.to(count).foreach { index =>
        array(y1 + index * dy)(x1 + index * dx) += 1
    }
  }
}

object Part2 {

  def mkVents(lines: Iterator[String]): Vents = {
    val lineSegmentPattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
    val lineSegments = lines.flatMap {
      case lineSegmentPattern(x1String, y1String, x2String, y2String) =>
        val x1 = x1String.toInt
        val y1 = y1String.toInt
        val x2 = x2String.toInt
        val y2 = y2String.toInt
        val dx = math.abs(x1 - x2)
        val dy = math.abs(y1 - y2)

        if (x1 == x2) Some(new VerticalLineSegment(x1, y1, x2, y2))
        else if (y1 == y2) Some(new HorizontalLineSegment(x1, y1, x2, y2))
        else if (dx == dy) Some(new DiagonalLineSegment(x1, y1, x2, y2))
        else None
      case _ => ???
    }.toVector

    new Vents(lineSegments)
  }

  def run(resourceName: String): Int = {
    val lines = Source
        .fromResource(resourceName)
        .getLines
    val vents = mkVents(lines)
    val overlap = vents.getOverlap

    overlap
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day5/input.txt")

  println(result)
}

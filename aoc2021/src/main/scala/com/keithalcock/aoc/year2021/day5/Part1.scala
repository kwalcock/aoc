package com.keithalcock.aoc.year2021.day5

import scala.io.Source

abstract class LineSegment(val x1: Int, val y1: Int, val x2: Int, val y2: Int) {
  def draw(array: Array[Array[Int]]): Unit
}

class HorizontalLineSegment(x1: Int, y1: Int, x2: Int, y2: Int) extends LineSegment(x1, y1, x2, y2) {

  def draw(array: Array[Array[Int]]): Unit = {
    (math.min(x1, x2)).to(math.max(x1, x2)).foreach { x =>
        array(y1)(x) += 1
    }
  }
}

class VerticalLineSegment(x1: Int, y1: Int, x2: Int, y2: Int) extends LineSegment(x1, y1, x2, y2) {

  def draw(array: Array[Array[Int]]): Unit = {
    (math.min(y1, y2)).to(math.max(y1, y2)).foreach { y =>
      array(y)(x1) += 1
    }
  }
}

class Vents(lineSegments: Seq[LineSegment]) {
  val maxX = math.max(lineSegments.maxBy(_.x1).x1, lineSegments.maxBy(_.x2).x2)
  val maxY = math.max(lineSegments.maxBy(_.y1).y1, lineSegments.maxBy(_.y2).y2)
  val array = {
    val array = Array.fill(maxY + 1, maxX + 1)(0)

    lineSegments.foreach(_.draw(array))
    array
  }

  def getOverlap: Int = {
    val overlap = array.map { row =>
        row.count(_ >= 2)
    }.sum

    overlap
  }
}

object Part1 {

  def mkVents(lines: Iterator[String]): Vents = {
    val lineSegmentPattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
    val lineSegments = lines.flatMap {
      case lineSegmentPattern(x1String, y1String, x2String, y2String) =>
        val x1 = x1String.toInt
        val y1 = y1String.toInt
        val x2 = x2String.toInt
        val y2 = y2String.toInt

        if (x1 == x2) Some(new VerticalLineSegment(x1, y1, x2, y2))
        else if (y1 == y2) Some(new HorizontalLineSegment(x1, y1, x2, y2))
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

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day5/input.txt")

  println(result)
}

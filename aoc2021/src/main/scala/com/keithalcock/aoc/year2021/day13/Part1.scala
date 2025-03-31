package com.keithalcock.aoc.year2021.day13

import com.keithalcock.aoc.year2021.Aoc
import com.keithalcock.aoc.year2021.utils.Point

sealed trait Fold {
  def apply(point: Point): Point
}

case class FoldX(x: Int) extends Fold {

  def apply(point: Point): Point = {
    if (point.x < x) point
    else Point(x + x - point.x, point.y)
  }
}

case class FoldY(y: Int) extends Fold {

  def apply(point: Point): Point = {
    if (point.y < y) point
    else Point(point.x, y + y - point.y)
  }
}

object Part1 extends Aoc[Int] {

  def getPointsFolds(lines: Iterator[String]): (List[Point], List[Fold]) = {
    val foldPattern = "fold along (x|y)=(\\d+)".r
    val points = lines
        .takeWhile(_.nonEmpty)
        .map { line =>
          val Array(x, y) = line.split(',').map(_.toInt)

          Point(x, y)
        }
        .toList
    val folds = lines.map {
      case foldPattern(xOrY, string) if xOrY == "x" => FoldX(string.toInt)
      case foldPattern(xOrY, string) if xOrY == "y" => FoldY(string.toInt)
    }.toList

    (points, folds)
  }

  def run(lines: Iterator[String]): Int = {
    val (points, folds) = getPointsFolds(lines)
    val fold = folds.head
    val newPoints = points.map { point =>
      fold(point)
    }.distinct

    newPoints.length
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day13/input.txt")

  println(result)
}

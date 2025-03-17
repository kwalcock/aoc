package com.keithalcock.aoc.year2021.day9

import scala.io.Source

case class Point(x: Int, y: Int) {

  def possibleNeighbors: List[Point] = {
    List(
      Point(x - 1, y + 0),
      Point(x + 1, y + 0),
      Point(x + 0, y - 1),
      Point(x + 0, y + 1)
    )
  }
}

class Cave(val heights: Array[Array[Int]], val width: Int) {
  val length = heights.length
  val xRange = 0.until(width)
  val yRange = 0.until(length)

  def getNeighbors(point: Point): List[Point] = {
    point.possibleNeighbors.filter { point =>
      xRange.contains(point.x) && yRange.contains(point.y)
    }
  }

  def heightAtPoint(point: Point): Int = heights(point.y)(point.x)

  def findLowPoints(): Seq[Point] = {
    val points = yRange.flatMap { y =>
      xRange.map { x =>
        Point(x, y)
      }
    }
    val lowPoints = points.filter { point =>
      val height = heightAtPoint(point)
      val neighbors = getNeighbors(point)

      neighbors.forall { neighbor =>
        height < heightAtPoint(neighbor)
      }
    }

    lowPoints
  }

  def calcRisk(): Int = {
    val lowPoints = findLowPoints()
    val lowHeights = lowPoints.map(heightAtPoint)
    val risk = lowHeights.sum + lowHeights.length

    risk
  }
}

object Part1 extends App {

  def mkCave(lines: Array[String]): Cave = {
    val width = lines.head.length
    val arrays = lines.map { line =>
      require(line.length == width)
      line.toArray.map(_ - '0')
    }

    new Cave(arrays, width)
  }

  def run(resourceName: String): Int = {
    val lines = Source
        .fromResource(resourceName)
        .getLines
        .toArray
    val cave = mkCave(lines)
    val risk = cave.calcRisk()

    risk
  }

  val result = run("com/keithalcock/aoc/year2021/day9/input.txt")

  println(result)
}

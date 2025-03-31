package com.keithalcock.aoc.year2021.day9

import com.keithalcock.aoc.year2021.Aoc
import com.keithalcock.aoc.year2021.utils.BasePoint

case class PointWithNeighbors(x: Int, y: Int) extends BasePoint {

  def possibleNeighbors: List[PointWithNeighbors] = {
    List(
      PointWithNeighbors(x - 1, y + 0),
      PointWithNeighbors(x + 1, y + 0),
      PointWithNeighbors(x + 0, y - 1),
      PointWithNeighbors(x + 0, y + 1)
    )
  }
}

class Cave(val heights: Array[Array[Int]], val width: Int) {
  val length = heights.length
  val xRange = 0.until(width)
  val yRange = 0.until(length)

  def getNeighbors(point: PointWithNeighbors): List[PointWithNeighbors] = {
    point.possibleNeighbors.filter { case PointWithNeighbors(x, y) =>
      xRange.contains(x) && yRange.contains(y)
    }
  }

  def heightAtPoint(point: PointWithNeighbors): Int = heights(point.y)(point.x)

  def findLowPoints(): Seq[PointWithNeighbors] = {
    val points = yRange.flatMap { y =>
      xRange.map { x =>
        PointWithNeighbors(x, y)
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

object Part1 extends Aoc[Int] {

  def mkCave(lines: Array[String]): Cave = {
    val width = lines.head.length
    val arrays = lines.map { line =>
      require(line.length == width)
      line.toArray.map(_ - '0')
    }

    new Cave(arrays, width)
  }

  def run(lines: Iterator[String]): Int = {
    val linesArray = lines.toArray
    val cave = mkCave(linesArray)
    val risk = cave.calcRisk()

    risk
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day9/input.txt")

  println(result)
}

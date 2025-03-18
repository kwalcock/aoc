package com.keithalcock.aoc.year2021.day11

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

case class Point(x: Int, y: Int) {

  def possibleNeighbors: Seq[Point] = {
    -1.to(1).flatMap { dy =>
      -1.to(1).flatMap { dx =>
        if (dx != 0 || dy != 0) Some(Point(x + dx, y + dy))
        else None
      }
    }
  }

  def isValid(range: Range): Boolean = {
    range.contains(x) && range.contains(y)
  }
}

class Grid(energyLevels: Array[Array[Int]]) {
  val dim = energyLevels.length
  val range = 0.until(dim)

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[Grid] && {
      val thatGrid = other.asInstanceOf[Grid]

      dim == thatGrid.dim && {
        range.forall { y =>
          range.forall { x =>
            val point = Point(x, y)

            getEnergyLevel(point) == thatGrid.getEnergyLevel(point)
          }
        }
      }
    }
  }

  def getEnergyLevel(point: Point): Int = energyLevels(point.y)(point.x)

  def setEnergyLevel(point: Point, value: Int): Int = {
    energyLevels(point.y)(point.x) = value
    value
  }

  def incEnergyLevel(point: Point): Int = setEnergyLevel(point, getEnergyLevel(point) + 1)

  def resetEnergyLevel(point: Point): Int = setEnergyLevel(point, 0)

  def step: Int = {
    val toFlash: ArrayBuffer[Point] = ArrayBuffer.empty
    val flashed: ArrayBuffer[Point] = ArrayBuffer.empty

    range.foreach { y =>
      range.foreach { x =>
        val point = Point(x, y)

        if (incEnergyLevel(point) == 10)
          toFlash += point
      }
    }

    @annotation.tailrec
    def loop(toFlash: ArrayBuffer[Point]): Unit = {
      if (toFlash.nonEmpty) {
        val point = toFlash.head
        val neighbors = point.possibleNeighbors.filter(_.isValid(range))

        neighbors.foreach { neighbor =>
          if (incEnergyLevel(neighbor) == 10)
            toFlash += neighbor
        }
        flashed += point
        loop(toFlash.tail)
      }
    }

    loop(toFlash)
    flashed.foreach { point =>
      resetEnergyLevel(point)
    }
    flashed.length
  }
}

object Part1 {

  def mkGrid(lines: Iterator[String]): Grid = {
    val linesList = lines.toList
    val grid = linesList.map { line =>
      line.map(_.toString.toInt).toArray
    }.toArray

    val width = linesList.head.length
    val height = linesList.length

    require(width == height)
    grid.foreach { line =>
      require(line.length == height)
    }
    new Grid(grid)
  }

  def mkGrid(source: Source): Grid = {
    val lines = source.getLines
    val grid = mkGrid(lines)

    grid
  }

  def mkGrid(resourceName: String): Grid = {
    Using.resource(Source.fromResource(resourceName)) { source =>
      mkGrid(source)
    }
  }

  def run(resourceName: String, count: Int): Long = {
    val grid = mkGrid(resourceName)
    val flashCount = 0.until(count).map(_ => grid.step).sum

    flashCount
  }
}

object Part1App extends App {
  val count = 100
  val result = Part1.run("com/keithalcock/aoc/year2021/day11/input.txt", count)

  println(result)
}

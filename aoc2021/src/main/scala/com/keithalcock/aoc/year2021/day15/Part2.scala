package com.keithalcock.aoc.year2021.day15

import com.keithalcock.aoc.year2021.Aoc
import com.keithalcock.aoc.year2021.utils.Point

import scala.collection.mutable.PriorityQueue

object Part2 extends Aoc[Int] {

  def translateDeltaCost(deltaCost: Int, translation: Int): Int = {
    val newDeltaCost = deltaCost + translation

    if (newDeltaCost > 9) newDeltaCost - 9 else newDeltaCost
  }

  def mkCells(lines: Iterator[String]): Seq[Cell] = {
    val smallArray = lines.map { line =>
      val smallDeltaCosts = line.toSeq.map(_.toString.toInt).toArray
      val largeDeltaCosts = Range(0, 5).map { xTranslation =>
        smallDeltaCosts.map(translateDeltaCost(_, xTranslation))
      }.toArray.flatten

      largeDeltaCosts
    }.toArray
    val largeArray = Range(0, 5).flatMap { yTranslation =>
      val newSmallArray = smallArray.map { deltaCosts =>
        deltaCosts.map(translateDeltaCost(_, yTranslation))
      }

      newSmallArray
    }.toArray
    val array = largeArray
    val xDim = array(0).length
    val yDim = array.length
    val xRange = Range(0, xDim)
    val yRange = Range(0, yDim)

    array.foreach { vector =>
      require(vector.length == xDim)
    }

    val cells = Array.tabulate(xDim, yDim) { (x, y) =>
      Cell(Point(x, y), array(y)(x))
    }

    val flattenedCells = 0.until(yDim).flatMap { y =>
      0.until(xDim).map { x =>
        val cell = cells(y)(x)
        val neighbors = Seq(
          if (xRange.contains(x - 1)) Some(cells(y)(x - 1)) else None,
          if (xRange.contains(x + 1)) Some(cells(y)(x + 1)) else None,
          if (yRange.contains(y - 1)) Some(cells(y - 1)(x)) else None,
          if (yRange.contains(y + 1)) Some(cells(y + 1)(x)) else None
        ).flatten

        neighbors.foreach(cell.addNeighbor)
        cell
      }
    }

    flattenedCells.head.mkStart()
    flattenedCells
  }

  def run(lines: Iterator[String]): Int = {
    val cells = mkCells(lines)
    val prioritizedCells = PriorityQueue(cells: _*)(CellOrdering)

    Part1.findEnd(prioritizedCells, cells.last)

    val cost = cells.last.getCost

    cost
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day15/input.txt")

  println(result)
}

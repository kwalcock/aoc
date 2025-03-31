package com.keithalcock.aoc.year2021.day15

import com.keithalcock.aoc.year2021.Aoc
import com.keithalcock.aoc.year2021.utils.Point

import scala.collection.mutable.PriorityQueue

class Cell(val point: Point, deltaCost: Int) {
  var neighbors: List[Cell] = List.empty
  var bestSourceOpt: Option[Cell] = None
  var lowestCostOpt: Option[Int] = None
  var visited: Boolean = false

  override def equals(other: Any): Boolean = {
    point == other.asInstanceOf[Cell].point
  }

  override def hashCode: Int = point.hashCode

  def addNeighbor(cell: Cell): Unit = neighbors = cell :: neighbors

  def getCost: Int = lowestCostOpt.get

  // Return true if the cost has been lowered.
  def updateSource(cell: Cell): Boolean = {
    val newCost = cell.getCost + deltaCost

    if (lowestCostOpt.isEmpty || newCost < lowestCostOpt.get) {
      bestSourceOpt = Some(cell)
      lowestCostOpt = Some(newCost)
      true
    }
    else false
  }

  def mkStart(): Cell = {
    bestSourceOpt = Some(this)
    lowestCostOpt = Some(0)
    this
  }
}

object CellOrdering extends Ordering[Cell] {

  override def compare(x: Cell, y: Cell): Int = {
    val lowToHigh = (x.lowestCostOpt, y.lowestCostOpt) match {
      case (None, None) => 0
      case (Some(_), None) => -1
      case (None, Some(_)) => +1
      case (Some(left), Some(right)) => left.compare(right)
    }
    val highToLow = -lowToHigh

    highToLow
  }
}

object Part1 extends Aoc[Int] {

  def mkCells(lines: Iterator[String]): Seq[Cell] = {
    val array = lines.map { line =>
      line.toSeq.map(_.toString.toInt).toArray
    }.toArray
    val xDim = array(0).length
    val yDim = array.length
    val xRange = Range(0, xDim)
    val yRange = Range(0, yDim)

    array.foreach { vector =>
      require(vector.length == xDim)
    }

    val cells = Array.tabulate(xDim, yDim) { (x, y) =>
      new Cell(Point(x, y), array(y)(x))
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

  def findEnd(prioritizedCells: PriorityQueue[Cell], end: Cell): Unit = {

    @annotation.tailrec
    def loop(prioritizedCells: PriorityQueue[Cell]): Unit = {
      val head = prioritizedCells.dequeue

      if (head != end) {
        val updated = head.neighbors.map { neighbor =>
          neighbor.updateSource(head)
        }
        val newPrioritizedCells =
            if (updated.contains(true)) PriorityQueue(prioritizedCells.dequeueAll: _*)(CellOrdering)
            else prioritizedCells

        loop(newPrioritizedCells)
      }
    }

    loop(prioritizedCells)
  }

  def run(lines: Iterator[String]): Int = {
    val cells = mkCells(lines)
    val prioritizedCells = PriorityQueue(cells: _*)(CellOrdering)

    findEnd(prioritizedCells, cells.last)

    val cost = cells.last.getCost

    cost
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day15/input.txt")

  println(result)
}

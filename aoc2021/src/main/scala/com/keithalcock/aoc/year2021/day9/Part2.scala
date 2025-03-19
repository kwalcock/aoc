package com.keithalcock.aoc.year2021.day9

import com.keithalcock.aoc.year2021.Aoc

class CaveWithBasins(heights: Array[Array[Int]], width: Int) extends Cave(heights, width) {

  def calcBasinSize(point: Point): Int = {

    @annotation.tailrec
    def loop(pointsToVisit: List[Point], visited: Set[Point]): Int = {
      if (pointsToVisit.isEmpty) visited.size
      else {
        val pointToVisit = pointsToVisit.head

        if (visited(pointToVisit))
          loop(pointsToVisit.tail, visited)
        else {
          val basinNeighbors = getNeighbors(pointToVisit).filter { neighbor =>
            heightAtPoint(neighbor) != 9
          }

          loop(pointsToVisit.tail ++ basinNeighbors, visited + pointToVisit)
        }
      }
    }

    loop(List(point), Set.empty)
  }

  def calcBasinScore(): Int = {
    val lowPoints = findLowPoints()
    val basinSizes = lowPoints.map(calcBasinSize)
    val product = basinSizes.sorted.reverse.take(3).product

    product
  }
}

object Part2 extends Aoc[Int] {

  def mkCaveWithBasins(lines: Array[String]): CaveWithBasins = {
    val width = lines.head.length
    val arrays = lines.map { line =>
      require(line.length == width)
      line.toArray.map(_ - '0')
    }

    new CaveWithBasins(arrays, width)
  }

  def run(lines: Iterator[String]): Int = {
    val linesArray = lines.toArray
    val cave = mkCaveWithBasins(linesArray)
    val risk = cave.calcBasinScore()

    risk
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day9/input.txt")

  println(result)
}

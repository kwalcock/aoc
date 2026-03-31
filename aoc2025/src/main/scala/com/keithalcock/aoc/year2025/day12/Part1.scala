package com.keithalcock.aoc.year2025.day12

import com.keithalcock.aoc.year2025.Aoc

object Part1 extends Aoc[Int]:

  case class Point(x: Int, y: Int):

    def rotatedClockwise: Point = Point(-y, x)

    def rotatedCounterclockwise: Point = Point(y, -x)

    def rotated: Point = rotatedClockwise

    def flippedVertically: Point = Point(x, -y)

    def flippedHorizontally: Point = Point(-x, y)

    def flipped: Point = flippedVertically

    def moveBy(other: Point): Point = Point(x + other.x, y + other.y)

  case class Shape(points: List[Point]):
    lazy val width =
      val minX = points.minBy(_.x).x
      val maxX = points.maxBy(_.x).x

      maxX - minX + 1
    lazy val length =
      val minY = points.minBy(_.y).y
      val maxY = points.maxBy(_.y).y

      maxY - minY + 1

    // In order to distinguish shapes properly, the list of points needs to be sorted.
    def sorted: Shape = Shape(points.sortBy(point => (point.x, point.y)))

    def rotated: Shape = Shape(points.map(_.rotated)).sorted.snugged

    def flipped: Shape = Shape(points.map(_.flipped)).sorted.snugged

    def snugged: Shape =
      val minPoint = Point(-points.minBy(_.x).x, -points.minBy(_.y).y)
      val newPoints = points.map(_.moveBy(minPoint))

      Shape(newPoints)

    def variations: Seq[Shape] =
      val rotations = 1.to(3).scanLeft(this): (shape, index) =>
       shape.rotated
      val flippeds = 1.to(3).scanLeft(flipped): (shape, index) =>
        shape.rotated
      val all = (rotations ++ flippeds).distinct

      all

    def moveBy(point: Point): Shape =
      Shape(points.map(_.moveBy(point)))

  case class Region(width: Int, length: Int, countIndexPairs: List[(Int, Int)]):

    def canFit(shapeVariations: Seq[Seq[Shape]]): Boolean =
      val board = Array.fill(length)(Array.fill(width)(false))
      val offPoint = Point(-1, -1)
      // For each variation, keep track of where it was last positioned.
      // When it is next positioned, try only down and right from it.
      val mostRecentPointsMatrix = shapeVariations
          .map: variations =>
            variations.map(_ => List(offPoint)).toArray
          .toArray

      def loop(countIndexPairs: List[(Int, Int)]): Boolean = {
        println(countIndexPairs)
        if countIndexPairs.isEmpty then
          true
        else
          val (count, index) = countIndexPairs.head
          val variations = shapeVariations(index)
          val result =
            val newCountIndexPairs =
              if count == 1 then
                countIndexPairs.tail
              else
                (count - 1, index) :: countIndexPairs.tail
            val result = variations.indices.exists: variationIndex =>
              val variation = variations(variationIndex)
              val mostRecentPointsList = mostRecentPointsMatrix(index)(variationIndex)
              val mostRecentPoint = mostRecentPointsList.head
              val yRange = math.max(mostRecentPoint.y, 0).to(length - variation.length)
              val xRange = 0.to(width - variation.width)
              val result = yRange.exists: y =>
                xRange.exists: x =>
                  val isForward = y > mostRecentPoint.y || y == mostRecentPoint.y && x > mostRecentPoint.x

                  isForward && {
                    val intersects = variation.points.exists: point =>
                      board(point.y + y)(point.x + x)
                    println(s"x = $x, y = $y")
                    !intersects && {
                      variation.points.foreach: point =>
                        board(point.y + y)(point.x + x) = true
                      mostRecentPointsMatrix(index)(variationIndex) = Point(x, y) :: mostRecentPointsList

                      val result = loop(newCountIndexPairs)

                      mostRecentPointsMatrix(index)(variationIndex) = mostRecentPointsList
                      variation.points.foreach: point =>
                        board(point.y + y)(point.x + x) = false
                      result
                    }
                  }
              result
            result
          result
      }

      loop(countIndexPairs)
    end canFit

  def run(lines: Iterator[String]): Int =
    val stanzas = IArray.from(lines.mkString("\n").split("\n\n"))
    val shapes = stanzas.init.map: string =>
      val points = string.linesIterator.drop(1).toSeq.zipWithIndex.flatMap:
        case (line, y) =>
          line.zipWithIndex.flatMap:
            case (char, x) => Option.when(char == '#')(Point(x, y))

      Shape(points.toList).sorted
    val shapeVariations = shapes.map(_.variations)
    val regions = stanzas.last.linesIterator
      .map: line =>
        val Array(dimensionsString, countsString) = line.split(":")
        val Array(width, height) = dimensionsString.split("x").map(_.toInt)
        val countIndexPairs = countsString.trim.split(" ").map(_.toInt).zipWithIndex.filter(sth => sth._1 != 0).toList

        Region(width, height, countIndexPairs)
    val fits = regions.toSeq.map: region =>
        val result = region.canFit(shapeVariations)
        println(result)
        result
    val result = fits.count(_ == true)

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day12/input.txt")

  println(result)

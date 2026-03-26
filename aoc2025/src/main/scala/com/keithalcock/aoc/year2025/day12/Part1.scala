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

      def loop(countIndexPairs: List[(Int, Int)], occupied: Set[Point]): Boolean =
        if countIndexPairs.isEmpty then
          true
        else
          val (count, index) = countIndexPairs.head
          val variations = shapeVariations(index)
          val shapeSize = variations.head.points.size
          val regionSize = width * length - occupied.size
          val noSpace = shapeSize > regionSize
          if noSpace then
            println("Nothing left")
          val result = shapeSize <= regionSize && {
            val newCountIndexPairs =
              if count == 1 then
                countIndexPairs.tail
              else
                (count - 1, index) :: countIndexPairs.tail
            val result = variations.exists: variation =>
              val xRange = 0.to(width - variation.width)
              val yRange = 0.to(length - variation.length)
              val result = xRange.exists: x =>
                yRange.exists: y =>
                  val movedShape = variation.moveBy(Point(x, y))
                  val intersects = movedShape.points.exists(occupied.contains)

                  !intersects && {
                    val newOccupied = occupied ++ movedShape.points

                    loop(newCountIndexPairs, newOccupied)
                  }
              result
            result
          }
          result

      loop(countIndexPairs, Set.empty)
    end canFit

  def run(lines: Iterator[String]): Int =
    val stanzas = lines.mkString("\n").split("\n\n")
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

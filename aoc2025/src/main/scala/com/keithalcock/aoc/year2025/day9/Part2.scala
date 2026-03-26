package com.keithalcock.aoc.year2025.day9

import com.keithalcock.aoc.year2025.Aoc

import scala.collection.BitSet
import scala.collection.mutable.BitSet as MutableBitSet

object Part2 extends Aoc[Long]:

  // Do I just need a list of all the squares that are white?
  // Make sure it doesn't intersect any.


  def mkRange(value1: Int, value2: Int): Range =
    value1.to(value2, -value1.compareTo(value2))

  case class Coordinate(x: Int, y: Int):

    def area(other: Coordinate): Long =
      (x - other.x + 1).abs.toLong * (y - other.y + 1).abs.toLong

  def allColored(bitSet: BitSet, width: Int, coordinate1: Coordinate, coordinate2: Coordinate): Boolean =
    val xRange = mkRange(coordinate1.x, coordinate2.x)
    val yRange = mkRange(coordinate1.y, coordinate2.y)

    xRange.forall: x =>
      yRange.forall: y =>
        bitSet(y * width + x)

  def run(lines: Iterator[String]): Long =
    val coordinates = lines
      .map:
        case s"$x,$y" => Coordinate(x.toInt, y.toInt)
      .toSeq
    val width = coordinates.maxBy(_.x).x
    val length = coordinates.maxBy(_.y).y
    val bitSet =
      val bitSet = new MutableBitSet(width * length)
      bitSet.add(coordinates.head.y + width + coordinates.head.x)
      coordinates.foldLeft(coordinates.head): (previous, current) =>
        if previous.x == current.x then // vertical line
          require(previous.y != current.y)
          val yRange = mkRange(previous.y, current.y)
          val x = current.x
          yRange.foreach: y =>
            bitSet.add(y * width + x)
        else
          require(previous.y == current.y) // horizontal line
          val xRange = mkRange(previous.x, current.x)
          val y = current.y
          xRange.foreach: x =>
            bitSet.add(y * width + x)
        current
      // TODO: Need to wrap back around to the first coordinate at the end.
      // Then the interiors need to be filled in somehow.
      bitSet.toImmutable
    val combinations = coordinates
      .combinations(2)
      .maxBy:
        case Seq(coordinate1, coordinate2) =>
          if allColored(bitSet, width, coordinate1, coordinate2) then
            coordinate1.area(coordinate2)
          else
            0
    val result = coordinates.head.area(coordinates.last)

    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day9/input.txt")

  println(result)

package com.keithalcock.aoc.year2025.day8

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec
import scala.collection.mutable.Set as MutableSet
import scala.io.Source
import scala.util.Using

object Part1 extends Aoc[Long]:

  case class DistanceRecord(index1: Int, index2: Int, distance: Long)

  type Circuit = MutableSet[Int]

  case class JunctionBox(x: Int, y: Int, z: Int):

    def distance(other: JunctionBox): Long =
      val xDiff = x.toLong - other.x
      val yDiff = y.toLong - other.y
      val zDiff = z.toLong - other.z
      val distance = xDiff * xDiff + yDiff * yDiff + zDiff * zDiff

      distance

  def connect(limit: Int, junctionBoxes: Array[JunctionBox]): List[Circuit] =
    val distanceRecords = junctionBoxes.indices.toArray.flatMap: index1 =>
      val junctionBox1 = junctionBoxes(index1)
      junctionBoxes.indices.drop(index1 + 1).map: index2 =>
        val junctionBox2 = junctionBoxes(index2)
        val distance = junctionBox1.distance(junctionBox2)

        DistanceRecord(index1, index2, distance)
    val sortedDistanceRecords = distanceRecords.sortBy(_.distance)

    @tailrec
    def loop(limit: Int, index: Int, circuits: List[Circuit]): List[Circuit] =
      if limit == 0 then circuits
      else
        val distanceRecord = sortedDistanceRecords(index)
        val circuit1Opt = circuits.find(_(distanceRecord.index1))
        val circuit2Opt = circuits.find(_(distanceRecord.index2))

        (circuit1Opt, circuit2Opt) match
          case (Some(circuit1), Some(circuit2)) if circuit1.eq(circuit2) =>
            // They are already in the same circuit, so just move on.  Do count the (indirect) connection.
            loop(limit - 1, index + 1, circuits)
          case (Some(circuit1), Some(circuit2)) =>
            // They are in different circuits, so combine the two circuits.
            // Do this by adding circuit2 to 1 and then filtering out circuit2.
            circuit1 ++= circuit2
            val newCircuits = circuits.filterNot(_.eq(circuit2))
            loop(limit - 1, index + 1, newCircuits)
          case (Some(circuit1), None) =>
            circuit1 += distanceRecord.index2
            loop(limit - 1, index + 1, circuits)
          case (None, Some(circuit2)) =>
            circuit2 += distanceRecord.index1
            loop(limit - 1, index + 1, circuits)
          case (None, None) =>
            val newCircuit = MutableSet(distanceRecord.index1, distanceRecord.index2)
            loop(limit - 1, index + 1, circuits :+ newCircuit)
    end loop

    loop(limit, 0, List.empty[Circuit])
  end connect

  def newJunctionBoxes(lines: Iterator[String]): Array[JunctionBox] =
    lines
      .map: line =>
        val Array(x, y, z) = line.split(",").map(_.toInt)
        JunctionBox(x, y, z)
      .toArray

  def score(circuits: List[Circuit]): Long =
    val largestCircuits = circuits.sortBy(-_.size)
    val largestSizes = largestCircuits.take(3).map(_.size.toLong)
    val result = largestSizes.product

    result

  def run(limit: Int, lines: Iterator[String]): Long =
    val junctionBoxes = newJunctionBoxes(lines)
    val circuits = connect(limit, junctionBoxes)
    val result = score(circuits)

    result

  def run(lines: Iterator[String]): Long =
    val junctionBoxes = newJunctionBoxes(lines)
    val circuits = connect(junctionBoxes.length, junctionBoxes)
    val result = score(circuits)

    result

  def run(limit: Int, source: Source): Long = run(limit, source.getLines)

  def run(limit: Int, resourceName: String): Long =
    Using.resource(Source.fromResource(resourceName)): source =>
      run(limit, source)

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day8/input.txt")

  println(result)

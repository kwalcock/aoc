package com.keithalcock.aoc.year2025.day8

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec
import scala.collection.mutable.Set as MutableSet

object Part2 extends Aoc[Long]:

  case class DistanceRecord(index1: Int, index2: Int, distance: Long)

  type Circuit = MutableSet[Int]

  case class JunctionBox(x: Int, y: Int, z: Int):

    def distance(other: JunctionBox): Long =
      val xDiff = x.toLong - other.x
      val yDiff = y.toLong - other.y
      val zDiff = z.toLong - other.z
      val distance = xDiff * xDiff + yDiff * yDiff + zDiff * zDiff

      distance

  def connect(limit: Int, junctionBoxes: Array[JunctionBox]): DistanceRecord =
    val distanceRecords = junctionBoxes.indices.toArray.flatMap: index1 =>
      val junctionBox1 = junctionBoxes(index1)
      junctionBoxes.indices.drop(index1 + 1).map: index2 =>
        val junctionBox2 = junctionBoxes(index2)
        val distance = junctionBox1.distance(junctionBox2)

        DistanceRecord(index1, index2, distance)
    val sortedDistanceRecords = distanceRecords.sortBy(_.distance)

    @tailrec
    def loop(index: Int, lastDistanceRecordOpt: Option[DistanceRecord], circuits: List[Circuit]): DistanceRecord =
      if index == sortedDistanceRecords.length then lastDistanceRecordOpt.get
      else
        val distanceRecord = sortedDistanceRecords(index)
        val circuit1Opt = circuits.find(_(distanceRecord.index1))
        val circuit2Opt = circuits.find(_(distanceRecord.index2))

        (circuit1Opt, circuit2Opt) match
          case (Some(circuit1), Some(circuit2)) if circuit1.eq(circuit2) =>
            // They are already in the same circuit, so just move on.  Do count the (indirect) connection.
            // Since the circuits haven't changed, stick with the lastDistanceRecordOpt.
            loop(index + 1, lastDistanceRecordOpt, circuits)
          case (Some(circuit1), Some(circuit2)) =>
            // They are in different circuits, so combine the two circuits.
            // Do this by adding circuit2 to 1 and then filtering out circuit2.
            circuit1 ++= circuit2
            val newCircuits = circuits.filterNot(_.eq(circuit2))
            loop(index + 1, Some(distanceRecord), newCircuits)
          case (Some(circuit1), None) =>
            circuit1 += distanceRecord.index2
            loop(index + 1, Some(distanceRecord), circuits)
          case (None, Some(circuit2)) =>
            circuit2 += distanceRecord.index1
            loop(index + 1, Some(distanceRecord), circuits)
          case (None, None) =>
            val newCircuit = MutableSet(distanceRecord.index1, distanceRecord.index2)
            // An additional circuit is made, so this new one can't be responsible for creating the single circuit.
            loop(index + 1, None, circuits :+ newCircuit)
    end loop

    loop(0, None, List.empty[Circuit])
  end connect

  def run(lines: Iterator[String]): Long =
    val junctionBoxes = lines
      .map: line =>
        val IArray(x, y, z) = line.isplit(',').map(_.toInt)
        JunctionBox(x, y, z)
      .toArray
    val distanceRecord = connect(junctionBoxes.length, junctionBoxes)
    val result = junctionBoxes(distanceRecord.index1).x.toLong * junctionBoxes(distanceRecord.index2).x.toLong

    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day8/input.txt")

  println(result)

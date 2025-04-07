package com.keithalcock.aoc.year2021.day19

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.ArrayBuffer

case class Point3d(x: Int, y: Int, z: Int) {

  def add(point: Point3d): Point3d = Point3d(x + point.x, y + point.y, z + point.z)
}

object Point3d {
  val origin = Point3d(0, 0, 0)
}

object Orientation {
  type Orientation = Point3d => Point3d

  val orientations: Seq[Orientation] = Seq(
    // +x direction
    (point: Point3d) => Point3d(+point.x, +point.y, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.x, -point.z, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.x, -point.y, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(+point.x, +point.z, -point.y), // at +270 degrees cw

    // -x direction
    (point: Point3d) => Point3d(-point.x, -point.y, +point.z), // rotated correctly
    (point: Point3d) => Point3d(-point.x, +point.z, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(-point.x, +point.y, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.x, -point.z, -point.y), // at +270 degrees cw

    // +y direction
    (point: Point3d) => Point3d(-point.y, +point.x, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.z, +point.x, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.y, +point.x, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.z, +point.x, -point.y), // at +270 degrees cw

    // -y direction
    (point: Point3d) => Point3d(+point.y, -point.x, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.z, -point.x, -point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(-point.y, -point.x, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.z, -point.x, +point.y), // at +270 degrees cw

    // +z direction
    (point: Point3d) => Point3d(-point.z, +point.y, +point.x), // rotated correctly
    (point: Point3d) => Point3d(+point.y, +point.z, +point.x), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.z, -point.y, +point.x), // at +180 degress cw
    (point: Point3d) => Point3d(-point.y, -point.z, +point.x), // at +270 degrees cw

    // -z direction
    (point: Point3d) => Point3d(-point.z, -point.y, -point.x), // rotated correctly
    (point: Point3d) => Point3d(-point.y, +point.z, -point.x), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.z, +point.y, -point.x), // at +180 degress cw
    (point: Point3d) => Point3d(+point.y, -point.z, -point.x)  // at +270 degrees cw
  )
}

case class Beacon(point: Point3d)

class Scanner(val beacons: Set[Beacon]) {

  def mkRange(values: Set[Int]): Range = {
    Range.inclusive(values.min, values.max)
  }

  val xRange = mkRange(beacons.map(_.point.x))
  val yRange = mkRange(beacons.map(_.point.y))
  val zRange = mkRange(beacons.map(_.point.z))

}

class DisorientedScanner(beacons: Set[Beacon]) extends Scanner(beacons) {

  def orient(orientation: Orientation.Orientation): OrientedScanner = {
    val newBeacons =
        if (orientation == Orientation.orientations.head) beacons
        else beacons.map(beacon => Beacon(orientation(beacon.point)))

    new OrientedScanner(Point3d.origin, orientation, newBeacons)
  }
}

case class SearchResult(orientation: Orientation.Orientation, translation: Point3d, disorientedScanner: DisorientedScanner, orientedScanner: OrientedScanner)

class OrientedScanner(point: Point3d, orientation: Part1.Orientation, beacons: Set[Beacon]) extends Scanner(beacons) {

  def orient(point: Point3d): OrientedScanner = {
    val newBeacons = beacons.map { beacon =>
      Beacon(beacon.point.add(point))
    }

    new OrientedScanner(point, orientation, newBeacons)
  }

  def orient(disorientedScanner: DisorientedScanner): Option[OrientedScanner] = {

    def translationRange(thisRange: Range, thatRange: Range): Range = {
      Range.inclusive(thisRange.min - thatRange.max, thisRange.max - thatRange.min)
    }

    def overlaps(orientedScanner: OrientedScanner): Boolean = {
      val intersection = beacons.intersect(orientedScanner.beacons)

      intersection.size >= 12
    }

    val searchResults = Orientation.orientations.flatMap { orientation =>
      val partiallyOrientedScanner = disorientedScanner.orient(orientation)
      val translationXRange = translationRange(this.xRange, partiallyOrientedScanner.xRange)
      val translationYRange = translationRange(this.yRange, partiallyOrientedScanner.yRange)
      val translationZRange = translationRange(this.zRange, partiallyOrientedScanner.zRange)

      translationXRange.flatMap { translationX =>
        translationYRange.flatMap { translationY =>
          translationZRange.flatMap { translationZ =>
            val translationPoint = Point3d(translationX, translationY, translationZ)
            val fullyOrientedScanner = partiallyOrientedScanner.orient(translationPoint)

            if (overlaps(fullyOrientedScanner))
              Some(SearchResult(orientation, translationPoint, disorientedScanner, fullyOrientedScanner))
            else None
          }
        }
      }
    }

    if (searchResults.size == 1)
      Some(searchResults.head.orientedScanner)
    else
      None
  }
}

object Part1 extends Aoc[Int] {
  type Orientation = Point3d => Point3d

  val orientations: Seq[Orientation] = Seq(
    // +x direction
    (point: Point3d) => Point3d(+point.x, +point.y, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.x, -point.z, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.x, -point.y, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(+point.x, +point.z, -point.y), // at +270 degrees cw

    // -x direction
    (point: Point3d) => Point3d(-point.x, -point.y, +point.z), // rotated correctly
    (point: Point3d) => Point3d(-point.x, +point.z, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(-point.x, +point.y, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.x, -point.z, -point.y), // at +270 degrees cw

    // +y direction
    (point: Point3d) => Point3d(-point.y, +point.x, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.z, +point.x, +point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.y, +point.x, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.z, +point.x, -point.y), // at +270 degrees cw

    // -y direction
    (point: Point3d) => Point3d(+point.y, -point.x, +point.z), // rotated correctly
    (point: Point3d) => Point3d(+point.z, -point.x, -point.y), // at  +90 degrees cw
    (point: Point3d) => Point3d(-point.y, -point.x, -point.z), // at +180 degress cw
    (point: Point3d) => Point3d(-point.z, -point.x, +point.y), // at +270 degrees cw

    // +z direction
    (point: Point3d) => Point3d(-point.z, +point.y, +point.x), // rotated correctly
    (point: Point3d) => Point3d(+point.y, +point.z, +point.x), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.z, -point.y, +point.x), // at +180 degress cw
    (point: Point3d) => Point3d(-point.y, -point.z, +point.x), // at +270 degrees cw

    // -z direction
    (point: Point3d) => Point3d(-point.z, -point.y, -point.x), // rotated correctly
    (point: Point3d) => Point3d(-point.y, +point.z, -point.x), // at  +90 degrees cw
    (point: Point3d) => Point3d(+point.z, +point.y, -point.x), // at +180 degress cw
    (point: Point3d) => Point3d(+point.y, -point.z, -point.x)  // at +270 degrees cw
  )

  def mkBeacons(lines: Iterator[String]): Set[Beacon] = {
    val stanza = lines.takeWhile(_.nonEmpty).toArray

    assert(stanza.head.startsWith("--- scanner "))
    assert(stanza.head.endsWith(" ---"))

    val beacons = stanza.tail.map { line =>
      val Array(x, y, z) = line.split(',')

      Beacon(Point3d(Integer.parseInt(x), Integer.parseInt(y), Integer.parseInt(z)))
    }
    val beaconSet = beacons.toSet

    require(beacons.length == beaconSet.size)
    beaconSet
  }

  def mkDisorientedScanners(lines: Iterator[String]): Seq[DisorientedScanner] = {
    val arrayBuffer = new ArrayBuffer[DisorientedScanner]()

    while (lines.hasNext) {
      val beacons = mkBeacons(lines)
      val disorientedScanner = new DisorientedScanner(beacons)

      arrayBuffer.append(disorientedScanner)
    }
    arrayBuffer
  }

  def orient(orientedScanners: Seq[OrientedScanner], disorientedScanners: Seq[DisorientedScanner]): (Seq[OrientedScanner], Seq[DisorientedScanner]) = {
    val pairs = orientedScanners.flatMap { orientedScanner =>
      disorientedScanners.map { disorientedScanner =>
        (orientedScanner, disorientedScanner)
      }
    }
    val pairOpt = pairs.find { case (orientedScanner, disorientedScanner) =>
      orientedScanner.orient(disorientedScanner).isDefined
    }

    if (pairOpt.isDefined) {
      val newOrientedScanner = pairOpt.get._1.orient(pairOpt.get._2).get
      val newOrientedScanners = orientedScanners :+ newOrientedScanner
      val newDisorientedScanners = disorientedScanners.filter(_ != pairOpt.get._2)

      (newOrientedScanners, newDisorientedScanners)
    }
    else {
      println("It didn't work.")
      throw new RuntimeException("Error")
    }
  }

  def run(lines: Iterator[String]): Int = {
    var (orientedScanners, disorientedScanners) = {
      val disorientedScanners = mkDisorientedScanners(lines)
      val orientedScanner = disorientedScanners.head.orient(orientations.head)

      (Seq(orientedScanner), disorientedScanners.tail)
    }

    while (disorientedScanners.nonEmpty) {
      val (newOrientedScanners, newDisorientedScanners) = orient(orientedScanners, disorientedScanners)

      orientedScanners = newOrientedScanners
      disorientedScanners = newDisorientedScanners
    }

    val beacons = orientedScanners.flatMap(_.beacons).toSet

    beacons.size
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day19/input.txt")

  println(result)
}

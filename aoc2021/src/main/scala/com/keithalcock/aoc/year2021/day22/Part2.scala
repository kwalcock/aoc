package com.keithalcock.aoc.year2021.day22

import com.keithalcock.aoc.year2021.Aoc

class Universe(xRanges: Seq[Range], yRanges: Seq[Range], zRanges: Seq[Range]) {
  val xs = xRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val ys = yRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val zs = zRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val xsLength = xs.length - 1
  val ysLength = ys.length - 1
  val zsLength = zs.length - 1
  val sizes = Array.fill(xsLength * ysLength * zsLength)(0L)
  val ons = Array.fill(xsLength * ysLength * zsLength)(false)

  0.until(xsLength).foreach { xIndex =>
    val xLength = xs(xIndex + 1) - xs(xIndex)

    0.until(ysLength).foreach { yIndex =>
      val yLength = ys(yIndex + 1) - ys(yIndex)

      0.until(zsLength).foreach { zIndex =>
        val zLength = zs(zIndex + 1) - zs(zIndex)
        val size = xLength.toLong * yLength * zLength
        val flatIndex = toFlatIndex(xIndex, yIndex, zIndex)

        sizes(flatIndex) = size
      }
    }
  }

  def toFlatIndex(xIndex: Int, yIndex: Int, zIndex: Int): Int = {
    xIndex * ysLength * zsLength +
    yIndex * zsLength +
    zIndex
  }

  def getRange(range: Range, ends: Seq[Int]): Range = {
    val indices = ends.indices.dropRight(1).filter { endIndex =>
      val start = ends(endIndex)
      val end = ends(endIndex + 1)

      range.start <= start && end <= range.end
    }
    val result = Range.inclusive(indices.head, indices.last)

    result
  }

  def execute(command: Command): Unit = {
    val on = command.on
    val xRange = getRange(command.xRange, xs)
    val yRange = getRange(command.yRange, ys)
    val zRange = getRange(command.zRange, zs)

    xRange.foreach { xIndex =>
      yRange.foreach { yIndex =>
        zRange.foreach { zIndex =>
          val flatIndex = toFlatIndex(xIndex, yIndex, zIndex)

          ons(flatIndex) = on
        }
      }
    }
  }

  def getCount: Long = {
    var count = 0L

    ons.indices.foreach { index =>
      val on = ons(index)

      if (on)
        count += sizes(index)
    }

    count
  }
}

object Part2 extends Aoc[Long] {
  val regex = "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

  def run(lines: Iterator[String]): Long = {
    val commands = lines.map {
      case regex(onOff, xStart, xEnd, yStart, yEnd, zStart, zEnd) =>
        Command(onOff == "on",
          Range(Integer.parseInt(xStart), Integer.parseInt(xEnd) + 1),
          Range(Integer.parseInt(yStart), Integer.parseInt(yEnd) + 1),
          Range(Integer.parseInt(zStart), Integer.parseInt(zEnd) + 1),
        )
    }.toArray
    val xRanges = commands.map(_.xRange)
    val yRanges = commands.map(_.yRange)
    val zRanges = commands.map(_.zRange)
    val universe = new Universe(xRanges, yRanges, zRanges)

    commands.foreach {command =>
      universe.execute(command)
    }

    val result = universe.getCount

    result
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day22/input.txt")

  println(result)
}

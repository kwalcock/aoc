package com.keithalcock.aoc.year2021.day22

import com.keithalcock.aoc.year2021.Aoc

class Quadrant(val xRange: Range, val yRange: Range, val zRange: Range) {
  protected var on: Boolean = false
  val size: Long = xRange.length.toLong * yRange.length * zRange.length

  def getOn: Boolean = on

  def setOn(on: Boolean): Unit = this.on = on

  def isInside(other: Quadrant): Boolean = {
    other.xRange.start <= xRange.start && xRange.end <= other.xRange.end &&
    other.yRange.start <= yRange.start && yRange.end <= other.yRange.end &&
    other.zRange.start <= zRange.start && zRange.end <= other.zRange.end
  }
}

class Universe(xRanges: Seq[Range], yRanges: Seq[Range], zRanges: Seq[Range]) {
  val xs = xRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val ys = yRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val zs = zRanges.flatMap { range => Seq(range.start, range.end) }.distinct.sorted
  val quadrants = xs.sliding(2).flatMap { case Seq(xStart, xEnd) =>
    val xRange = Range(xStart, xEnd)

    ys.sliding(2).flatMap { case Seq(yStart, yEnd) =>
      val yRange = Range(yStart, yEnd)

      zs.sliding(2).map { case Seq(zStart, zEnd) =>
        val zRange = Range(zStart, zEnd)

        new Quadrant(xRange, yRange, zRange)
      }
    }
  }.toArray

  def execute(command: Command): Unit = {
    val commandQuadrant = new Quadrant(command.xRange, command.yRange, command.zRange)

    quadrants.foreach { quadrant =>
      if (quadrant.isInside(commandQuadrant))
        quadrant.setOn(command.on)
    }
  }

  def getCount: Long = {
    val result = quadrants.map { quadrant =>
      if (quadrant.getOn) quadrant.size else 0
    }.sum

    result
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

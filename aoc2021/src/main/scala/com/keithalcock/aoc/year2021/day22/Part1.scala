package com.keithalcock.aoc.year2021.day22

import com.keithalcock.aoc.year2021.Aoc

case class Command(on: Boolean, xRange: Range, yRange: Range, zRange: Range)

class Space(range: Range) {
  val booleans = Array.fill(range.length, range.length, range.length)(false)
  var count = 0

  def getCount: Int = count

  def get(x: Int, y: Int, z: Int): Boolean = {
    booleans(x - range.start)(y - range.start)(z - range.start)
  }

  def set(x: Int, y: Int, z: Int, value: Boolean): Unit = {
    booleans(x - range.start)(y - range.start)(z - range.start) = value
  }

  def shrink(range: Range): Range = {
    val overlap = this.range.intersect(range)

    if (overlap.nonEmpty) {
      val start = overlap.min
      val end = overlap.max

      Range.inclusive(start, end)
    }
    else Range(0, 0)
  }

  def execute(command: Command): Unit = {
    val shrunkenXRange = shrink(command.xRange)
    val shrunkenYRange = shrink(command.yRange)
    val shrunkenZRange = shrink(command.zRange)

    shrunkenXRange.foreach { x =>
      shrunkenYRange.foreach { y =>
        shrunkenZRange.foreach { z =>
          val oldValue = get(x, y, z)
          val newValue = command.on

          if (oldValue != newValue) {
            set(x, y, z, newValue)
            if (newValue) count += 1
            else count -= 1
          }
        }
      }
    }
  }
}

object Part1 extends Aoc[Int] {
  val spaceRange = Range.inclusive(-50, 50)
  val regex = "(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)".r

  def run(lines: Iterator[String]): Int = {
    val commands = lines.map {
      case regex(onOff, xStart, xEnd, yStart, yEnd, zStart, zEnd) =>
        Command(onOff == "on",
          Range.inclusive(Integer.parseInt(xStart), Integer.parseInt(xEnd)),
          Range.inclusive(Integer.parseInt(yStart), Integer.parseInt(yEnd)),
          Range.inclusive(Integer.parseInt(zStart), Integer.parseInt(zEnd)),
        )
    }.toArray
    val space = new Space(spaceRange)

    commands.foreach {command =>
      space.execute(command)
    }

    val result = space.getCount

    result
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day22/input.txt")

  println(result)
}

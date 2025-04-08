package com.keithalcock.aoc.year2021.day20

import com.keithalcock.aoc.year2021.Aoc

class Rule(line: String) {
  val map = line.map { char =>
    char == '#'
  }

  def apply(value: Int): Boolean = map(value)
}

class Scan(val values: Array[Array[Boolean]], val default: Boolean) {
  val width = values.head.length
  val length = values.length

  def calcValue(values: Array[Array[Boolean]], x: Int, y: Int, width: Int, length: Int): Int = {
    val bits = Range.inclusive(-1, 1).flatMap { dy =>
      val newY = y + dy
      val lights = Range.inclusive(-1, 1).map { dx =>
        val newX = x + dx
        val light =
            if (0 <= newY && newY < length && 0 <= newX && newX < width) values(newY)(newX)
            else default

        light
      }

      lights
    }
    val string = bits.map { value => if (value) '1' else '0'}.mkString
    val value = Integer.parseInt(string, 2)

    value
  }

  def apply(rule: Rule): Scan = {
    val tops = Array(Array.fill(width + 2)(default))
    val mids = values.map { row =>
      default +: row :+ default
    }
    val bots = Array(Array.fill(width + 2)(default))
    val expandedValues = tops ++ mids ++ bots

    val newValues = expandedValues.indices.map { yIndex =>
      expandedValues(yIndex).indices.map { xIndex =>
        val value = calcValue(expandedValues, xIndex, yIndex, width + 2, length + 2)

        rule(value)
      }.toArray
    }.toArray

    new Scan(newValues, if (!default) rule(0) else rule(511))
  }

  def count: Int = {
    val sum = values.map { row =>
      row.count(_ == true)
    }.sum

    sum
  }
}

object Part1 extends Aoc[Int] {

  def mkRule(lines: Iterator[String]): Rule = {
    val line = lines.next
    val rule = new Rule(line)

    lines.next
    rule
  }

  def mkScan(lines: Iterator[String]): Scan = {
    val values = lines.map { line =>
      line.map { char =>
        char == '#'
      }.toArray
    }.toArray

    new Scan(values, false)
  }

  def run(lines: Iterator[String]): Int = {
    val rule = mkRule(lines)
    val scan0 = mkScan(lines)
    val scan1 = scan0.apply(rule)
    val scan2 = scan1.apply(rule)
    val count = scan2.count

    count
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day20/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2025.day1

import scala.io.Source
import scala.util.Using

object Part1:
  val size = 100

  case class Rotation(value: Int):

    def this(direction: Char, amount: Int) =
      this {
        val value = direction match
          case 'L' => Rotation.normalize(-amount)
          case 'R' => Rotation.normalize(amount)
          case _ => ???

        require(amount >= 0)
        assert(0 <= value && value < size)
        value
      }

    def this(string: String) =
      this(string.head, string.tail.toInt)

  object Rotation:

    def normalize(value: Int): Int =
      val modded = value % size

      if modded < 0 then
        modded + size
      else
        modded

  def run(start: Int, resourceName: String): Int =
    Using.resource(Source.fromResource(resourceName)): source =>
      run(start, source)

  def run(start: Int, source: Source): Int =
    run(start, source.getLines)

  def run(start: Int, lines: Iterator[String]): Int =
    val count = lines
        .map: line =>
          new Rotation(line)
        .scanLeft(start): (current: Int, rotation: Rotation) =>
          Rotation.normalize(current + rotation.value)
        .count(_ == 0)

    count

object Part1App extends App:
  val result = Part1.run(50, "com/keithalcock/aoc/year2025/day1/input.txt")

  println(result)

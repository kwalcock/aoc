package com.keithalcock.aoc.year2025.day1

import scala.io.Source
import scala.util.Using

case class Rotation(value: Int):

  def this(size: Int, direction: Char, amount: Int) = {
    this {
      val value = direction match
        case 'L' => amount % size
        case 'R' => (-(amount % size) + size) % size
        case _ => ???

      require(amount >= 0)
      assert(0 <= value && value < size)
      value
    }
  }

  override def toString(): String =
    if value >= 0 then
      s"L$value"
    else
      s"R${value.abs}"

object Part1:
  val size = 100

  def run(start: Int, resourceName: String): Int =
    Using.resource(Source.fromResource(resourceName)): source =>
      run(start, source)

  def run(start: Int, source: Source): Int =
    run(start, source.getLines)

  def run(start: Int, lines: Iterator[String]): Int =
    val count = lines
        .map: line =>
          new Rotation(size, line.head, line.tail.toInt)
        .scanLeft(start): (current: Int, rotation: Rotation) =>
          (current + rotation.value) % size
        .count(_ == 0)

    count

object Part1App extends App:
  val result = Part1.run(50, "com/keithalcock/aoc/year2025/day1/input.txt")

  println(result)


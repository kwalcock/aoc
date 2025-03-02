package com.keithalcock.aoc.year2021.day3

import scala.io.Source

class GasMonitor(bitCount: Int, binaryValues: Array[String]) {
  import GasMonitor._

  val length = binaryValues.length

  def next(position: Int, winChar: Char, loseChar: Char): GasMonitor = {
    val zeroCount = binaryValues.count { binaryValue =>
      binaryValue.charAt(position) == zeroChar
    }
    val oneCount = length - zeroCount
    // val o2Winner = if (oneCount >= zeroCount) oneChar else zeroChar
    // val co2Winner = if (zeroCount <= oneCount) zeroChar else oneChar
    // val co2Winner = if (oneCount >= zeroCount) zeroChar else oneChar
    val winner = if (oneCount >= zeroCount) winChar else loseChar
    val newBinaryValues = binaryValues.filter { binaryValue =>
      binaryValue.charAt(position) == winner
    }

    new GasMonitor(bitCount, newBinaryValues)
  }

  def toInt: Int = {
    require(length == 1)
    Integer.parseInt(binaryValues.head, 2)
  }
}

object GasMonitor {
  val zeroChar = '0'
  val oneChar = '1'

  def apply(lines: Iterator[String]): GasMonitor = {
    val bufferedLines = lines.buffered
    val bitCount = bufferedLines.head.length
    val array = bufferedLines.toArray
    val gasMonitor = new GasMonitor(bitCount, array)

    gasMonitor
  }

  @annotation.tailrec
  def loop(gasMonitor: GasMonitor, position: Int, winChar: Char, loseChar: Char): Int = {
    if (gasMonitor.length == 1)
      gasMonitor.toInt
    else
      loop(gasMonitor.next(position, winChar, loseChar), position + 1, winChar, loseChar)
  }

  def rateO2(gasMonitor: GasMonitor) = loop(gasMonitor, 0, GasMonitor.oneChar, GasMonitor.zeroChar)

  def rateCo2(gasMonitor: GasMonitor) = loop(gasMonitor, 0, GasMonitor.zeroChar, GasMonitor.oneChar)
}

object Part2 extends App {
  val lines = Source
      .fromResource("com/keithalcock/aoc/year2021/day3/input.txt")
      .getLines
  val gasMonitor = GasMonitor(lines)
  val o2 = GasMonitor.rateO2(gasMonitor)
  val co2 = GasMonitor.rateCo2(gasMonitor)
  val product = o2 * co2

  println(product)
}

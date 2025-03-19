package com.keithalcock.aoc.year2021.day3

import com.keithalcock.aoc.year2021.Aoc

class BitCounter(bitCount: Int) {
  import BitCounter._

  val zeroCounts = Array.fill(bitCount)(0)
  var counts = 0

  def count(iterator: Iterator[String]): BitCounter = {
    iterator.foreach(count)
    this
  }

  def count(bits: String): Unit = {
    counts += 1
    bits.indices.foreach { index =>
      if (bits(index) == zeroChar)
        zeroCounts(index) += 1
    }
  }

  def toInt(bits: Array[Char]): Int = {
    Integer.parseInt(String.valueOf(bits), 0, bitCount, 2)
  }

  def mkGammaChars(): Array[Char] = Array.tabulate(bitCount) { index =>
    val moreOnes = 2 * zeroCounts(index) < counts

    if (moreOnes) oneChar else zeroChar
  }

  def mkEpsilonChars(): Array[Char] = mkGammaChars.map { char =>
    ('0' + '1' - char).toChar
  }

  def calcGamma: Int = toInt(mkGammaChars())

  def calcEpsilon: Int = toInt(mkEpsilonChars())
}

object BitCounter {
  val zeroChar = '0'
  val oneChar = '1'

  def apply(lines: Iterator[String]): BitCounter = {
    val bufferedLines = lines.buffered
    val bitCounter = new BitCounter(bufferedLines.head.length)

    bitCounter.count(bufferedLines)
  }
}

object Part1 extends Aoc[Int]{

  def run(lines: Iterator[String]): Int = {
    val bitCounter = BitCounter(lines)
    val gamma = bitCounter.calcGamma
    val epsilon = bitCounter.calcEpsilon
    val product = gamma * epsilon

    product
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day3/input.txt")

  println(result)
}

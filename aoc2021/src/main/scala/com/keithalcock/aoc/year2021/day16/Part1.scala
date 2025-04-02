package com.keithalcock.aoc.year2021.day16

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.ArrayBuffer

abstract class Packet(val length: Int, val version: Int, val typeId: Int) {

  def versionSum: Int
}

object Packet {
  type Bit = Byte

  def apply(bits: Iterator[Bit]): Packet = {
    val version = Packet.toInt(Packet.takeNext(bits, 3))
    val typeId = Packet.toInt(Packet.takeNext(bits, 3))
    val length = 6

    if (typeId == 4)
      LiteralValue(length, version, typeId, bits)
    else
      Operator(length, version, typeId, bits)
  }

  def toLong(bits: Array[Bit]): Long = {
    val string = bits.map { bit =>
      if (bit == 1) '1' else '0'
    }.mkString
    val long = java.lang.Long.parseLong(string, 2)

    long
  }

  def toInt(bits: Array[Bit]): Int = toLong(bits).toInt

  def takeNext(bits: Iterator[Bit], count: Int): Array[Bit] = {
    val result = bits.take(count).toArray

    0.until(count).foreach(_ => bits.next())
    result
  }
}

class LiteralValue(length: Int, version: Int, typeId: Int, val value: Long) extends Packet(length, version, typeId) {

  def versionSum: Int = version
}

object LiteralValue {

  def apply(length: Int, version: Int, typeId: Int, bits: Iterator[Packet.Bit]): LiteralValue = {
    val quint = 5
    var zeroQuintupleOpt: Option[Seq[Packet.Bit]] = None
    val oneQuintuples = bits.sliding(quint, quint).takeWhile { quintuple =>
      val result = quintuple.head == 1

      if (!result)
        zeroQuintupleOpt = Some(quintuple)
      result
    }.toArray.flatten
    val quintuples = oneQuintuples ++ zeroQuintupleOpt.get
    val additionalLength = quintuples.length
    val valueBits = quintuples.sliding(quint, quint).flatMap { bits =>
      bits.slice(1, quint)
    }.toArray
    val value = Packet.toLong(valueBits)

    new LiteralValue(length + additionalLength, version, typeId, value)
  }
}

class Operator(length: Int, version: Int, typeId: Int, val children: Seq[Packet]) extends Packet(length, version, typeId) {

  def versionSum: Int = version + children.map(_.versionSum).sum
}

object Operator {

  def apply(length: Int, version: Int, typeId: Int, bits: Iterator[Packet.Bit]): Operator = {
    val lengthTypeId = Packet.takeNext(bits, 1).head

    if (lengthTypeId == 0) {
      val size = 15
      val additionalLength = 1 + size
      val operatorLength = Packet.toInt(Packet.takeNext(bits, size))
      val children = ArrayBuffer[Packet]()
      var remainingLength = operatorLength

      while (remainingLength > 0) {
        val packet = Packet(bits)

        children.append(packet)
        remainingLength -= packet.length
      }

      new Operator(length + additionalLength + operatorLength, version, typeId, children)
    }
    else {
      val size = 11
      val additionalLength = 1 + size
      val operatorCount = Packet.toInt(Packet.takeNext(bits, size))
      val children = Range(0, operatorCount).map { index =>
        Packet(bits)
      }
      val operatorLength = children.map(_.length).sum

      new Operator(length + additionalLength + operatorLength, version, typeId, children)
    }
  }
}

object Part1 extends Aoc[Int] {

  def toBits(string: String): Array[Byte] = {
    val bits = string.flatMap { char =>
      val int = Integer.parseInt(char.toString, 16)
      val bin = Integer.toBinaryString(int)
      val string = "0" * (4 - bin.length) + bin
      val bytes = string.map { char =>
        val byte: Byte = if (char == '1') 1 else 0

        byte
      }

      bytes
    }.toArray

    bits
  }

  def run(lines: Iterator[String]): Int = {
    val bits = toBits(lines.next)
    // val bits = toBits("D2FE28")
    // val bits = toBits("38006F45291200")
    // val bits = toBits("EE00D40C823060")
    val packet = Packet(bits.iterator)
    val versionSum = packet.versionSum

    versionSum
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day16/input.txt")

  println(result)
}

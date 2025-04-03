package com.keithalcock.aoc.year2021.day16

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Long] {

  def runString(string: String): Long = {
    val bits = Part1.toBits(string)
    val packet = Packet(bits.iterator)
    val result = packet.evaluate

    result
  }

  def run(lines: Iterator[String]): Long = {
    runString(lines.next())
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day16/input.txt")

  println(result)
}

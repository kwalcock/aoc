package com.keithalcock.aoc.year2021.day24

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.{Map => MutableMap}

object Part2 extends Aoc[Long] {

  def run(lines: Iterator[String]): Long = {
    val line = lines.mkString("\n")
    val stanzas = line.split("\n\n")
    val machines = stanzas.map { stanza =>
      val commands = stanza.split('\n').map { line =>
        Command(line)
      }

      new Machine(commands)
    }
    val inputArray = Array(0)
    var zMap = MutableMap(0L -> 0L) // z -> input

    machines.foreach { machine =>
      val newZMap = MutableMap[Long, Long]()

      Range.inclusive(1, 9).foreach { w =>
        zMap.foreach { case (oldZ, oldInput) =>
          inputArray(0) = w
          val newZ = machine.process(new Input(inputArray), oldZ)
          val newInput = oldInput * 10 + w
          val prevInputOpt = newZMap.get(newZ)

          if (prevInputOpt.isEmpty || newInput < prevInputOpt.get)
            newZMap(newZ) = newInput
        }
      }
      zMap = newZMap
    }

    val minZeroInput = zMap(0L)

    minZeroInput
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day24/input.txt")

  println(result)
}

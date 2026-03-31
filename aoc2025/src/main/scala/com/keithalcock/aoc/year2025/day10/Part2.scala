package com.keithalcock.aoc.year2025.day10

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part2 extends Aoc[Int]:

  case class Machine(joltages: IndexedSeq[Int], buttons: Array[Array[Int]], count: Int, counters: IndexedSeq[Int]):

    def this(joltages: IndexedSeq[Int], buttons: Array[Array[Int]], count: Int = 0) = this(joltages, buttons, count,
        IndexedSeq.fill(joltages.size)(0))

    def isSolved: Boolean = joltages == counters

    def pushButton(index: Int): Machine =
      val newCounters = buttons(index).foldLeft(counters): (counters, index) =>
        counters.updated(index, counters(index) + 1)

      Machine(joltages, buttons, count + 1, newCounters)

  def solve(machine: Machine): Int =

    @tailrec
    def loop(machines: Seq[Machine], seenCounters: Set[IndexedSeq[Int]]): Int =
      val machine = machines.head

      if machine.isSolved then {
        println(machine.count)
        machine.count
      } else
        val pushedMachines = machine.buttons.indices.map(machine.pushButton)
        val filteredMachines = pushedMachines.filterNot(machine => seenCounters.contains(machine.counters))
        val newMachines = machines.tail ++ filteredMachines
        val newSeenStates = seenCounters ++ filteredMachines.map(_.counters)

        loop(newMachines, newSeenStates)

    loop(Seq(machine), Set(machine.counters))

  def run(lines: Iterator[String]): Int =
    val result = lines
      .map: line =>
        val fields = line.split(" ")
        val buttons = fields.drop(1).dropRight(1).map: button =>
          button.drop(1).dropRight(1).split(",").map(_.toInt)
        val joltages = IArray.from(fields.last.drop(1).dropRight(1).split(",")).map(_.toInt)

        solve(new Machine(joltages, buttons))
      .sum

    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day10/input.txt")

  println(result)

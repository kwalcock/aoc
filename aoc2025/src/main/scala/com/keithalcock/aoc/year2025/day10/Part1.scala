package com.keithalcock.aoc.year2025.day10

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part1 extends Aoc[Int]:

  case class Machine(goal: Set[Int], buttons: IndexedSeq[IndexedSeq[Int]], count: Int = 0, state: Set[Int] = Set.empty[Int]):

    def isSolved: Boolean = goal == state

    def pushButton(index: Int): Machine =
      val newState = buttons(index).foldLeft(state): (state, index) =>
        if state.contains(index) then
          state - index
        else
          state + index

      Machine(goal, buttons, count + 1, newState)

  def solve(machine: Machine): Int =

    @tailrec
    def loop(machines: Seq[Machine], seenStates: Set[Set[Int]]): Int =
      val machine = machines.head

      if machine.isSolved then
        machine.count
      else
        val pushedMachines = machine.buttons.indices.map(machine.pushButton)
        val filteredMachines = pushedMachines.filterNot(machine => seenStates.contains(machine.state))
        val newMachines = machines.tail ++ filteredMachines
        val newSeenStates = seenStates ++ filteredMachines.map(_.state)

        loop(newMachines, newSeenStates)

    loop(Seq(machine), Set(machine.state))

  def run(lines: Iterator[String]): Int =
    val result = lines
      .map: line =>
        val fields = line.isplit(' ')
        val goal = fields.head.drop(1).dropRight(1).zipWithIndex.filter(_._1 == '#').map(_._2).toSet
        val buttons = fields.drop(1).dropRight(1).map: button =>
          button.drop(1).dropRight(1).isplit(',').map(_.toInt).toIndexedSeq

        solve(Machine(goal, buttons))
      .sum

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day10/input.txt")

  println(result)

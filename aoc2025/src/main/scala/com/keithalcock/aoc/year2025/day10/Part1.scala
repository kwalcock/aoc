package com.keithalcock.aoc.year2025.day10

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part1 extends Aoc[Int]:

  case class Machine(goal: Set[Int], buttons: Array[Array[Int]], count: Int = 0, state: Set[Int] = Set.empty[Int]):

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
    def loop(machines: Seq[Machine]): Int =
      val machine = machines.head

      if machine.isSolved then
        machine.count
      else
        val newMachines = machine.buttons.indices.map(machine.pushButton)
        val allMachines = machines.tail ++ newMachines

        loop(allMachines)

    loop(Seq(machine))

  def run(lines: Iterator[String]): Int =
    val result = lines
      .map: line =>
        val fields = line.split(" ")
        val goal = fields.head.drop(1).dropRight(1).zipWithIndex.filter(_._1 == '#').map(_._2).toSet
        val buttons = fields.drop(1).dropRight(1).map: button =>
          button.drop(1).dropRight(1).split(",").map(_.toInt)

        solve(Machine(goal, buttons))
      .sum

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day10/input.txt")

  println(result)

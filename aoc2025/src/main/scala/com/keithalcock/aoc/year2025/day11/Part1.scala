package com.keithalcock.aoc.year2025.day11

import com.keithalcock.aoc.year2025.Aoc

import scala.annotation.tailrec

object Part1 extends Aoc[Int]:

  case class Machine(name: String, outputs: Seq[String])

  type Path = List[Machine]
  type Paths = Seq[Path]

  def solve(machines: Map[String, Machine]): Paths =
    val you = machines("you")
    val out = Machine("out", Seq.empty)
    val allMachines = machines + ("out" -> out)

    @tailrec
    def loop(unfinished: Paths, finished: Paths): Paths =
      if unfinished.isEmpty then
        finished
      else
        val path = unfinished.head
        val machine = path.head
        val newPaths = machine.outputs.map: output =>
          allMachines(output) :: path
        val newFinisheds = newPaths.filter(_.head.eq(out))
        val newUnfinisheds = newPaths.filterNot(_.head.eq(out))

        loop(unfinished.tail ++ newUnfinisheds, newFinisheds ++ finished)

    loop(Seq(List(you)), Seq.empty)
  end solve

  def run(lines: Iterator[String]): Int =
    val machines = lines
      .map: line =>
        val fields = line.split(" ")
        val name = fields.head.init
        val outputs = fields.tail.toSeq

        name -> Machine(name, outputs)
      .toMap
    val paths = solve(machines)
    val result = paths.length

    result

object Part1App extends App:
  val result = Part1.run("com/keithalcock/aoc/year2025/day11/input.txt")

  println(result)

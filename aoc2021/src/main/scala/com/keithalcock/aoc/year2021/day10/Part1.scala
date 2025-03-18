package com.keithalcock.aoc.year2021.day10

import scala.io.Source

object Part1 {
  val data = Seq(
    ('(', ')', 3),
    ('[', ']', 57),
    ('{', '}', 1197),
    ('<', '>', 25137)
  )
  val pairs = data.map { case (left, right, score) => left -> right }.toMap
  val points = data.map { case (left, right, score) => right -> score }.toMap
  val lefts = pairs.keySet

  def getIllegalOpt(line: String): Option[Char] = {

    def loop(line: String, stack: List[Char]): Option[Char] = {
      if (line.isEmpty) None
      else {
        val char = line.head

        if (lefts.contains(char))
          loop(line.tail, pairs(char) :: stack)
        else if (stack.headOption.contains(char)) loop(line.tail, stack.tail)
        else Some(char)
      }
    }

    val illegalOpt = loop(line, List.empty)

    illegalOpt
  }

  def run(resourceName: String): Int = {
    val syntaxErrorScore = Source
        .fromResource(resourceName)
        .getLines
        .toList
        .flatMap(getIllegalOpt)
        .map(points)
        .sum

    syntaxErrorScore
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day10/input.txt")

  println(result)
}

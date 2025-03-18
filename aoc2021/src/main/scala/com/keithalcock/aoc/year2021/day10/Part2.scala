package com.keithalcock.aoc.year2021.day10

import com.keithalcock.aoc.year2021.Aoc

import scala.io.Source

object Part2 extends Aoc[Long] {
  val data = Seq(
    ('(', ')', 1),
    ('[', ']', 2),
    ('{', '}', 3),
    ('<', '>', 4)
  )
  val pairs = data.map { case (left, right, score) => left -> right }.toMap
  val points = data.map { case (left, right, score) => right -> score }.toMap
  val lefts = pairs.keySet

  def getCompletionOpt(line: String): Option[List[Char]] = {

    def loop(line: String, stack: List[Char]): Option[List[Char]] = {
      if (line.isEmpty)
        if (stack.isEmpty) None
        else Some(stack)
      else {
        val char = line.head

        if (lefts.contains(char))
          loop(line.tail, pairs(char) :: stack)
        else if (stack.headOption.contains(char)) loop(line.tail, stack.tail)
        else None
      }
    }

    val completionOpt = loop(line, List.empty)

    completionOpt
  }

  def scoreCompletion(completion: List[Char]): Long = {
    completion.foldLeft(0L) { case (score, completion) =>
      score * 5 + points(completion)
    }
  }

  def run(lines: Iterator[String]): Long = {
    val syntaxErrorScores = lines
        .toList
        .flatMap(getCompletionOpt)
        .map(scoreCompletion)
        .sorted

    require(syntaxErrorScores.length % 2 == 1)

    val middleScore = syntaxErrorScores.drop(syntaxErrorScores.length / 2).head

    middleScore
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day10/input.txt")

  println(result)
}

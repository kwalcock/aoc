package com.keithalcock.aoc.year2021.day10

import scala.io.Source

object Part2 extends App {
  lazy val data = Seq(
    ('(', ')', 1),
    ('[', ']', 2),
    ('{', '}', 3),
    ('<', '>', 4)
  )
  lazy val pairs = data.map { case (left, right, score) => left -> right }.toMap
  lazy val points = data.map { case (left, right, score) => right -> score }.toMap
  lazy val lefts = pairs.keySet

  def getCompletionOpt(line: String): Option[List[Char]] = {

    def loop(line: String, stack: List[Char]): Option[List[Char]] = {
      if (line.isEmpty)
        if (stack.isEmpty) None
        else Some(stack)
      else {
        val char = line.head

        if (lefts.contains(char))
          loop(line.tail, pairs(char) :: stack)
        else
          if (stack.headOption.contains(char)) loop(line.tail, stack.tail)
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

  def run(resourceName: String): Long = {
    val lazyScores = data
    val lazyPairs = pairs
    val lazyPoints = points
    val lazyLefts =lefts
    val syntaxErrorScores = Source
        .fromResource(resourceName)
        .getLines
        .toList
        .flatMap(getCompletionOpt)
        .map(scoreCompletion)
        .sorted

    require(syntaxErrorScores.length % 2 == 1)

    val middleScore = syntaxErrorScores.drop(syntaxErrorScores.length / 2).head

    middleScore
  }

  val result = run("com/keithalcock/aoc/year2021/day10/input.txt")

  println(result)
}

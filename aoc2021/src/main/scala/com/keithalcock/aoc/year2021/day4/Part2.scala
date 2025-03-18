package com.keithalcock.aoc.year2021.day4

import com.keithalcock.aoc.year2021.Aoc

import scala.io.Source

object Part2 extends Aoc[Int] {

  def play(moves: Array[Int], bingos: Array[Bingo]): Int = {

    @annotation.tailrec
    def loop(moves: Array[Int], bingos: Array[Bingo]): Int = {
      val move = moves.head
      val newBingos = bingos.map(_.move(move))
      val filteredNewBingos =
        if (newBingos.length > 1) newBingos.filterNot(_.isWinner)
        else newBingos

      if (filteredNewBingos.length == 1 && filteredNewBingos.head.isWinner)
        filteredNewBingos.head.score(move)
      else
        loop(moves.tail, filteredNewBingos)
    }

    val winningScore = loop(moves, bingos)

    winningScore
  }

  def run(lines: Iterator[String]): Int = {
    val bufferedLines = lines.buffered
    val moves = Bingo.readMoves(bufferedLines)
    val bingos = Bingo.readBingos(bufferedLines)
    val winningScore = play(moves, bingos)

    winningScore
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day4/input.txt")

  println(result)
}

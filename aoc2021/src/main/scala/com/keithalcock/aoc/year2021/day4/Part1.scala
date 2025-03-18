package com.keithalcock.aoc.year2021.day4

import scala.io.Source

class Bingo(rows: Array[Array[Int]], cols: Array[Array[Int]]) {

  def move(value: Int): Bingo = {
    val newRows = rows.map { row =>
      row.filterNot(_ == value)
    }
    val newCols = cols.map { col =>
      col.filterNot(_ == value)
    }

    new Bingo(newRows, newCols)
  }

  def isWinner: Boolean = {
    rows.exists(_.isEmpty) || cols.exists(_.isEmpty)
  }

  def score(value: Int): Int = {
    rows.map(_.sum).sum * value
  }
}

object Bingo {

  def newBingo(cells: Array[Array[Int]]): Bingo = {
    val rows = cells
    val cols = Array.tabulate(5, 5) { (col, row) =>
      cells(row)(col)
    }

    new Bingo(rows, cols)
  }

  def readMoves(lines: BufferedIterator[String]): Array[Int] = {
    val moves = lines.next.split(',').map(_.toInt)

    lines.next
    moves
  }

  def readBingos(lines: BufferedIterator[String]): Array[Bingo] = {

    @annotation.tailrec
    def loop(bingos: List[Bingo]): Array[Bingo] = {
      if (!lines.hasNext) bingos.reverse.toArray
      else {
        val rows = (0 until 5).map { _ =>
          val strings = lines.next.split(' ').filterNot(_.isEmpty)
          val ints = strings.map(_.toInt)

          ints
        }.toArray
        val bingo = newBingo(rows)

        if (lines.hasNext)
          lines.next
        loop(bingo :: bingos)
      }
    }

    val bingos = loop(List.empty)

    bingos
  }
}

object Part1 {

  def play(moves: Array[Int], bingos: Array[Bingo]): Int = {

    @annotation.tailrec
    def loop(moves: Array[Int], bingos: Array[Bingo]): Int = {
      val move = moves.head
      val newBingos = bingos.map { bingo =>
        bingo.move(move)
      }
      val winnerOpt = newBingos.find(_.isWinner)

      if (winnerOpt.isDefined)
        winnerOpt.get.score(move)
      else
        loop(moves.tail, newBingos)
    }

    val winningScore = loop(moves, bingos)

    winningScore
  }

  def run(resourceName: String): Int = {
    val lines = Source
        .fromResource(resourceName)
        .getLines
        .buffered
    val moves = Bingo.readMoves(lines)
    val bingos = Bingo.readBingos(lines)
    val winningScore = play(moves, bingos)

    winningScore
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day4/input.txt")

  println(result)
}

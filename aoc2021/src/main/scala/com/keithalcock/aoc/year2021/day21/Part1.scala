package com.keithalcock.aoc.year2021.day21

import com.keithalcock.aoc.year2021.Aoc

class Die {
  var current = 1
  var count = 0

  def roll: Int = {
    val result = current

    count += 1
    current = if (current < 100) current + 1 else 1
    result
  }
}

class Player(var space: Int) {
  var score: Int = 0

  def circle(): Unit = {
    while (space > 10)
      space -= 10
  }

  def move(one: Int, two: Int, three: Int): Unit = {
    space += one
    circle()

    space += two
    circle()

    space += three
    circle()

    score += space
  }
}

object Part1 extends Aoc[Int] {

  def mkSpace(line: String): Int = Integer.parseInt(line.split(' ').last)

  def play(player1: Player, player2: Player, die: Die): Unit = {

    while (player1.score < 1000 && player2.score < 1000) {
      player1.move(die.roll, die.roll, die.roll)

      if (player1.score < 1000)
        player2.move(die.roll, die.roll, die.roll)
    }
  }

  def run(lines: Iterator[String]): Int = {
    val die = new Die()
    val space1 = mkSpace(lines.next)
    val space2 = mkSpace(lines.next)
    val player1 = new Player(space1)
    val player2 = new Player(space2)

    play(player1, player2, die)

    val lowScore = math.min(player1.score, player2.score)
    val dieCount = die.count
    val result = lowScore * dieCount

    result
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day21/input.txt")

  println(result)
}

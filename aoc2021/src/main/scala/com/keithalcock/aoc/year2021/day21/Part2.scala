package com.keithalcock.aoc.year2021.day21

import com.keithalcock.aoc.year2021.Aoc

case class Roll(dots: Int, count: Int)

class QuantumDie {

  def roll: Seq[Roll] = QuantumDie.rolls
}

object QuantumDie {
  val rolls = Seq(
    Roll(3, 1),
    Roll(4, 3),
    Roll(5, 6),
    Roll(6, 7),
    Roll(7, 6),
    Roll(8, 3),
    Roll(9, 1)
  )
}

// SpaceOne is the 1-10 version.  SpaceZero is 0-9 for indexing.
case class PlayerState(var spaceOne: Int, var score: Int) {

  def circle(): Unit = {
    while (spaceOne > 10)
      spaceOne -= 10
  }

  def move(spaces: Int): Unit = {
    spaceOne = spaceOne + spaces
    circle()
    score += spaceOne
  }
}

class Game(space1: Int, space2: Int) {
  var player1WinCount = 0L
  var player2WinCount = 0L
  val spaceLength = 10 // 1 to 10
  val scoreLength = 21 // 0 to 20
  var oldCounts = Array.fill(spaceLength * scoreLength * spaceLength * scoreLength)(0L)
  val sizes = Array(
    scoreLength * spaceLength * scoreLength * 1,
    spaceLength * scoreLength * 1,
    scoreLength * 1,
    1
  )
  var remainingStates = 1
  val die = new QuantumDie()

  def calcIndex(player1SpaceZero: Int, player1Score: Int, player2SpaceZero: Int, player2Score: Int): Int = {
    player1SpaceZero * sizes(0) + player1Score * sizes(1) + player2SpaceZero * sizes(2) + player2Score * sizes(3)
  }

  def setCount(counts: Array[Long], player1SpaceZero: Int, player1Score: Int, player2SpaceZero: Int, player2Score: Int, count: Long): Unit = {
    val index = calcIndex(player1SpaceZero, player1Score, player2SpaceZero, player2Score)

    counts(index) = count
  }

  def getCount(counts: Array[Long], player1SpaceZero: Int, player1Score: Int, player2SpaceZero: Int, player2Score: Int): Long = {
    val index = calcIndex(player1SpaceZero, player1Score, player2SpaceZero, player2Score)

    counts(index)
  }

  def setCount(counts: Array[Long], player1State: PlayerState, player2State: PlayerState, count: Long): Unit = {
    setCount(counts, player1State.spaceOne - 1, player1State.score, player2State.spaceOne - 1, player2State.score, count)
  }

  def getCount(counts: Array[Long], player1State: PlayerState, player2State: PlayerState): Long = {
    getCount(counts, player1State.spaceOne - 1, player1State.score, player2State.spaceOne - 1, player2State.score)
  }

  {
    val player1State = PlayerState(space1, 0)
    val player2State = PlayerState(space2, 0)

    setCount(oldCounts, player1State, player2State, 1)
  }

  def play(): Unit = {
    while (remainingStates > 0) {
      // Move player1
      var newCounts = Array.fill(oldCounts.length)(0L)

      remainingStates = 0
      Range.inclusive(1, 10).foreach { player1SpaceOne =>
        Range(0, 21).foreach { player1Score =>
          Range.inclusive(1, 10).foreach { player2SpaceOne =>
            Range(0, 21).foreach { player2Score =>
              val player1State = PlayerState(player1SpaceOne, player1Score)
              val player2State = PlayerState(player2SpaceOne, player2Score)
              val oldCount = getCount(oldCounts, player1State, player2State)

              // Only do something for existing states.
              if (oldCount > 0) {
                val rolls = die.roll

                rolls.foreach { case Roll(rollSpaces, rollCount) =>
                  val player1State = PlayerState(player1SpaceOne, player1Score)
                  val deltaCount = oldCount * rollCount

                  player1State.move(rollSpaces)
                  if (player1State.score >= 21)
                    player1WinCount += deltaCount
                  else {
                    val existingNewCount = getCount(newCounts, player1State, player2State)
                    val newCount = existingNewCount + deltaCount

                    setCount(newCounts, player1State, player2State, newCount)

                    // Here's a new state that will need to be dealt with.
                    if (existingNewCount == 0)
                      remainingStates += 1
                  }
                }
              }
            }
          }
        }
      }

      oldCounts = newCounts
      if (remainingStates > 0) {
        // Move player2
        val newCounts = Array.fill(oldCounts.length)(0L)

        remainingStates = 0
        Range.inclusive(1, 10).foreach { player1SpaceOne =>
          Range(0, 21).foreach { player1Score =>
            val player1State = PlayerState(player1SpaceOne, player1Score)

            Range.inclusive(1, 10).foreach { player2SpaceOne =>
              Range(0, 21).foreach { player2Score =>
                val player2State = PlayerState(player2SpaceOne, player2Score)
                val oldCount = getCount(oldCounts, player1State, player2State)

                // Only do something for existing states.
                if (oldCount > 0) {
                  val rolls = die.roll

                  rolls.foreach { case Roll(rollSpaces, rollCount) =>
                    val player2State = PlayerState(player2SpaceOne, player2Score)
                    val deltaCount = oldCount * rollCount

                    player2State.move(rollSpaces)
                    if (player2State.score >= 21)
                      player2WinCount += deltaCount
                    else {
                      val existingNewCount = getCount(newCounts, player1State, player2State)
                      val newCount = existingNewCount + deltaCount

                      setCount(newCounts, player1State, player2State, newCount)

                      // Here's a new state that will need to be dealt with.
                      if (existingNewCount == 0)
                        remainingStates += 1
                    }
                  }
                }
              }
            }
          }
        }
        oldCounts = newCounts
      }
    }
  }
}

object Part2 extends Aoc[Long] {

  def run(lines: Iterator[String]): Long = {
    val space1 = Part1.mkSpace(lines.next)
    val space2 = Part1.mkSpace(lines.next)
    val game = new Game(space1, space2)

    game.play()

    val result = math.max(game.player1WinCount, game.player2WinCount)

    result
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day21/input.txt")

  println(result)
}

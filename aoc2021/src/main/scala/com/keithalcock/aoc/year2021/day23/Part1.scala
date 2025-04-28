package com.keithalcock.aoc.year2021.day23

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.{Map => MutableMap}

trait Space

case class Hallway(index: Int, nextToRoom: Boolean) extends Space
case class Room(index: Int, letter: Char, hallwayIndex: Int) extends Space
case object Wall extends Space

object Part1 extends Aoc[Int] {
  val A = 'A'
  val B = 'B'
  val C = 'C'
  val D = 'D'
  val E = '.' // E is for empty
  val W = '#' // W is for wall
  val spaces = Seq[Space](
    Room(0, A, 11), // 0 is the farthest inside, the wall side
    Room(1, A, 11), // 1 is next to the hallway, the door side
    Room(0, B, 13),
    Room(1, B, 13),
    Room(0, C, 15),
    Room(1, C, 15),
    Room(0, D, 17),
    Room(1, D, 17),
    Wall,
    Hallway(0, false),
    Hallway(1, false),
    Hallway(2, true),
    Hallway(3, false),
    Hallway(4, true),
    Hallway(5, false),
    Hallway(6, true),
    Hallway(7, false),
    Hallway(8, true),
    Hallway(9, false),
    Hallway(10, false),
    Wall
  )
  val costs = Map(
    A -> 1,
    B -> 10,
    C -> 100,
    D -> 1000
  )
  val scores: MutableMap[Seq[Char], Int] = MutableMap.empty
  val finisheds: MutableMap[Seq[Char], Boolean] = MutableMap.empty
  val win = Seq(A, A, B, B, C, C, D, D, W, E, E, E, E, E, E, E, E, E, E, E, W)

  def duplicate(occupants: Seq[Char], oldIndex: Int, newIndex: Int, letter: Char): Seq[Char] = {
    val newOccupants = occupants.zipWithIndex.map { case (occupant, index) =>
      if (index == oldIndex) E
      else if (index == newIndex) letter
      else occupant
    }

    newOccupants
  }

  def moveFromRoom0(occupantIndex: Int, occupants: Seq[Char], spaces: Seq[Space], oldScore: Int, scores: MutableMap[Seq[Char], Int], winScoreOpt: Option[Int]): Unit = {
    val occupantLetter = occupants(occupantIndex)
    val room = spaces(occupantIndex).asInstanceOf[Room]
    val Room(_, roomLetter, hallwayIndex) = room
    val correctRoom = occupantLetter == roomLetter

    if (!correctRoom && occupants(occupantIndex + 1) == E) {
      val steps = 2 // This gets as far as the hallway.

      // Only try to move out of room if it isn't the right letter
      // and the door is not blocked.  The hallway must always be free.
      assert(occupants(hallwayIndex) == E)

      { // Go the one way.
        var newSteps = steps
        var newHallwayIndex = hallwayIndex - 1

        while (occupants(newHallwayIndex) == E) {
          newSteps += 1

          if (!spaces(newHallwayIndex).asInstanceOf[Hallway].nextToRoom) {
            val newScore = oldScore + newSteps * costs(occupantLetter)

            if (winScoreOpt.isEmpty || newScore < winScoreOpt.get) {
              val newOccupants = duplicate(occupants, occupantIndex, newHallwayIndex, occupantLetter)
              val oldScoreOpt = scores.get(newOccupants)

              if (oldScoreOpt.isEmpty || oldScoreOpt.get > newScore) {
                scores(newOccupants) = newScore
                finisheds(newOccupants) = false
              }
            }
          }
          newHallwayIndex -= 1
        }
      }

      { // Go the other way.
        var newSteps = steps
        var newHallwayIndex = hallwayIndex + 1

        while (occupants(newHallwayIndex) == E) {
          newSteps += 1
          if (!spaces(newHallwayIndex).asInstanceOf[Hallway].nextToRoom) {
            val newScore = oldScore + newSteps * costs(occupantLetter)

            if (winScoreOpt.isEmpty || newScore < winScoreOpt.get) {
              val newOccupants = duplicate(occupants, occupantIndex, newHallwayIndex, occupantLetter)
              val oldScoreOpt = scores.get(newOccupants)

              if (oldScoreOpt.isEmpty || oldScoreOpt.get > newScore) {
                scores(newOccupants) = newScore
                finisheds(newOccupants) = false
              }
            }
          }
          newHallwayIndex += 1
        }
      }
    }
  }

  def moveFromRoom1(occupantIndex: Int, occupants: Seq[Char], spaces: Seq[Space], oldScore: Int, scores: MutableMap[Seq[Char], Int], winScoreOpt: Option[Int]): Unit = {
    val room = spaces(occupantIndex).asInstanceOf[Room]
    val Room(_, roomLetter, hallwayIndex) = room
    val occupantLetter = occupants(occupantIndex)
    val correctRoom = occupantLetter == roomLetter
    val otherCorrectRoom = occupants(occupantIndex - 1) == roomLetter

    if (!correctRoom || !otherCorrectRoom) {
      val steps = 1 // This gets as far as the hallway.

      // Only try to move out of room if it isn't the right letter
      // or the door is blocked for an incorrect letter.  The hallways must always be free.
      assert(occupants(hallwayIndex) == E)

      { // Do the one way.
        var newSteps = steps
        var newHallwayIndex = hallwayIndex - 1

        while (occupants(newHallwayIndex) == E) {
          newSteps += 1

          if (!spaces(newHallwayIndex).asInstanceOf[Hallway].nextToRoom) {
            val newScore = oldScore + newSteps * costs(occupantLetter)

            if (winScoreOpt.isEmpty || newScore < winScoreOpt.get) {
              val newOccupants = duplicate(occupants, occupantIndex, newHallwayIndex, occupantLetter)
              val oldScoreOpt = scores.get(newOccupants)

              if (oldScoreOpt.isEmpty || oldScoreOpt.get > oldScore) {
                scores(newOccupants) = newScore
                finisheds(newOccupants) = false
              }
            }
          }
          newHallwayIndex -= 1
        }
      }

      { // Go the other way.
        var newSteps = steps
        var newHallwayIndex = hallwayIndex + 1

        while (occupants(newHallwayIndex) == E) {
          newSteps += 1

          if (!spaces(newHallwayIndex).asInstanceOf[Hallway].nextToRoom) {
            val newScore = oldScore + newSteps * costs(occupantLetter)

            if (winScoreOpt.isEmpty || newScore < winScoreOpt.get) {
              val newOccupants = duplicate(occupants, occupantIndex, newHallwayIndex, occupantLetter)
              val oldScoreOpt = scores.get(newOccupants)

              if (oldScoreOpt.isEmpty || oldScoreOpt.get > newScore) {
                scores(newOccupants) = newScore
                finisheds(newOccupants) = false
              }
            }
          }
          newHallwayIndex += 1
        }
      }
    }
  }

  def moveFromHallway(occupantIndex: Int, occupants: Seq[Char], spaces: Seq[Space], oldScore: Int, scores: MutableMap[Seq[Char], Int], winScoreOpt: Option[Int]): Boolean = {
    val occupantLetter = occupants(occupantIndex)
    val wallRoom = spaces.find { space =>
      space.isInstanceOf[Room] && space.asInstanceOf[Room].letter == occupantLetter
    }.get.asInstanceOf[Room]
    val wallRoomIndex = spaces.indexOf(wallRoom)
    val doorRoomIndex = wallRoomIndex + 1
    val hallwayIndex = wallRoom.hallwayIndex

    assert(occupants(hallwayIndex) == E)
    if (occupants(doorRoomIndex) == E && (occupants(wallRoomIndex) == E || occupants(wallRoomIndex) == occupantLetter)) {
      // There must be a space to land and the wall space must be empty or already containing the correct occupant.
      // Always go in as far as possible, because then that occupant will be done.
      val freeway = (hallwayIndex < occupantIndex && Range(occupantIndex - 1, hallwayIndex, -1).forall(occupants(_) == E)) ||
                    (hallwayIndex > occupantIndex && Range(occupantIndex + 1, hallwayIndex, +1).forall(occupants(_) == E))

      if (freeway) {
        val roomSteps = if (occupants(wallRoomIndex) == E) 2 else 1
        val newSteps = math.abs(hallwayIndex - occupantIndex) + roomSteps
        val newRoomIndex = if (occupants(wallRoomIndex) == E) wallRoomIndex else doorRoomIndex
        val newScore = oldScore + newSteps * costs(occupantLetter)
        val newOccupants = duplicate(occupants, occupantIndex, newRoomIndex, occupantLetter)
        val oldScoreOpt = scores.get(newOccupants)

        if (oldScoreOpt.isEmpty || newScore < oldScoreOpt.get) {
          scores(newOccupants) = newScore
          finisheds(newOccupants) = false
          newOccupants == win
        }
        else false
      }
      else false
    }
    else false
  }

  def run(occupants: Seq[Char]): Int = {
    var winScoreOpt: Option[Int] = None
    scores(occupants) = 0
    finisheds(occupants) = false

    def loopOccupants(occupants: Seq[Char]): Unit = {
      val oldScore = scores(occupants)
      val indexes = occupants.indices.flatMap { index =>
        if (occupants(index) != E && occupants(index) != W) Some(index)
        else None
      }

      indexes.foreach { index =>
        spaces(index) match {
          case Room(roomIndex, _, _) =>
            if (roomIndex == 0)
              moveFromRoom0(index, occupants, spaces, oldScore, scores, winScoreOpt)
            else {
              assert(roomIndex == 1)
              moveFromRoom1(index, occupants, spaces, oldScore, scores, winScoreOpt)
            }
          case Hallway(_, nextToRoom) =>
            assert(!nextToRoom)
            val getsWon = moveFromHallway(index, occupants, spaces, oldScore, scores, winScoreOpt)

            if (getsWon) {
              val winScore = scores(win)

              winScoreOpt = Some(winScore)
              scores.foreach { case (occupants, score) =>
                if (score >= winScore)
                  finisheds(occupants) = true
              }
            }
        }
      }
      finisheds(occupants) = true
    }

    def loop(): Boolean = {
      val occupantsFinishedPairOpt = finisheds.find { case (occupants, boolean) =>
        !boolean && occupants != win
      }

      if (occupantsFinishedPairOpt.isDefined) {
        val occupants = occupantsFinishedPairOpt.get._1

        loopOccupants(occupants)
        true
      }
      else
        false
    }

    while (loop()) { }

    val result = scores(win)

    result
  }

  def run(lines: Iterator[String]): Int = {
    // This was converted from the input by hand.
    val occupants = Seq(B, D, A, A, D, B, C, C, W, E, E, E, E, E, E, E, E, E, E, E, W)
    val result = run(occupants)

    result
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day23/input.txt")

  println(result)
}

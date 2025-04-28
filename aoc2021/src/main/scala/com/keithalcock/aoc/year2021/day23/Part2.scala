package com.keithalcock.aoc.year2021.day23

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.{Map => MutableMap}

object Part2 extends Aoc[Int] {
  val A = 'A'
  val B = 'B'
  val C = 'C'
  val D = 'D'
  val E = '.' // E is for empty
  val W = '#' // W is for wall
  val spaces = Seq[Space](
    Room(0, A, 19), // 0 is the farthest inside, the wall side
    Room(1, A, 19), // 1 is next to the hallway, the door side
    Room(2, A, 19),
    Room(3, A, 19),

    Room(0, B, 21),
    Room(1, B, 21),
    Room(2, B, 21),
    Room(3, B, 21),

    Room(0, C, 23),
    Room(1, C, 23),
    Room(2, C, 23),
    Room(3, C, 23),

    Room(0, D, 25),
    Room(1, D, 25),
    Room(2, D, 25),
    Room(3, D, 25),

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
  val win = Seq(A, A, A, A, B, B, B, B, C, C, C, C, D, D, D, D, W, E, E, E, E, E, E, E, E, E, E, E, W)
  val wallRoomIndexes = costs.keys.map { letter =>
    val index = spaces.zipWithIndex.find { case (space, index) =>
      space.isInstanceOf[Room] && space.asInstanceOf[Room].letter == letter
    }.get._2

    letter -> index
  }.toMap

  def duplicate(occupants: Seq[Char], oldIndex: Int, newIndex: Int, letter: Char): Seq[Char] = {
    val newOccupants = occupants.zipWithIndex.map { case (occupant, index) =>
      if (index == oldIndex) E
      else if (index == newIndex) letter
      else occupant
    }

    newOccupants
  }

  def moveFromRoom(n: Int, occupantIndex: Int, occupants: Seq[Char], spaces: Seq[Space], oldScore: Int, scores: MutableMap[Seq[Char], Int], winScoreOpt: Option[Int]): Unit = {
    val occupantLetter = occupants(occupantIndex)
    val room = spaces(occupantIndex).asInstanceOf[Room]
    val Room(roomIndex, roomLetter, hallwayIndex) = room
    val correctRoom = occupantLetter == roomLetter
    val wallRoomIndex = wallRoomIndexes(roomLetter)
    val otherIncorrectRoom = Range(wallRoomIndex, occupantIndex).exists { index =>
      occupants(index) != roomLetter
    }

    if (!correctRoom || otherIncorrectRoom) {
      val doorRoomIndex = wallRoomIndex + 3
      val roomRange = Range.inclusive(occupantIndex + 1, doorRoomIndex)
      val roomOk = roomRange.forall { index =>
        val letter = occupants(index)

        letter == E
      }

      if (roomOk) {
        val steps = 4 - roomIndex

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
  }

  def moveFromHallway(occupantIndex: Int, occupants: Seq[Char], spaces: Seq[Space], oldScore: Int, scores: MutableMap[Seq[Char], Int], winScoreOpt: Option[Int]): Boolean = {
    val occupantLetter = occupants(occupantIndex)
    val wallRoomIndex = wallRoomIndexes(occupantLetter)
    val doorRoomIndex = wallRoomIndex + 3
    val roomRange = Range.inclusive(doorRoomIndex, wallRoomIndex, -1)
    val roomOk = roomRange.forall { index =>
      val letter = occupants(index)

      letter == occupantLetter || letter == E
    }

    if (roomOk) {
      val hallwayIndex = spaces(wallRoomIndex).asInstanceOf[Room].hallwayIndex
      val hallwayRange =
          if (hallwayIndex < occupantIndex) Range.inclusive(occupantIndex - 1, hallwayIndex, -1)
          else Range.inclusive(occupantIndex + 1, hallwayIndex, +1)
      val hallwayOk = hallwayRange.forall { index =>
        occupants(index) == E
      }

      assert(occupants(hallwayIndex) == E)
      if (hallwayOk) {
        val roomSteps = roomRange.takeWhile { index =>
          occupants(index) == E
        }.length
        val newSteps = math.abs(hallwayIndex - occupantIndex) + roomSteps
        val newRoomIndex = doorRoomIndex - roomSteps + 1
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
            moveFromRoom(roomIndex, index, occupants, spaces, oldScore, scores, winScoreOpt)
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
    val occupants = Seq(
      B, D, D, D,
      A, B, C, A,
      D, A, B, B,
      C, C, A, C,
      W, E, E, E, E, E, E, E, E, E, E, E, W
    )
    val result = run(occupants)

    result
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day23/input.txt")

  println(result)
}

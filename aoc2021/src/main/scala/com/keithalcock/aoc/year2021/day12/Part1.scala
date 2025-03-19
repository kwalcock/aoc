package com.keithalcock.aoc.year2021.day12

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.{ArrayBuffer, Set => MutableSet}

sealed trait Cave {
  def name: String
  def canRevisit: Boolean

  def isStart: Boolean = name.toLowerCase == "start"
  def isEnd: Boolean = name.toLowerCase == "end"
}

case class Small(name: String) extends Cave {
  override val canRevisit = false
}
case class Large(name: String) extends Cave {
  override val canRevisit = true
}

object Cave {

  def apply(name: String): Cave = {
    name match {
      case name if name.forall(_.isUpper) => Large(name)
      case name if name.forall(_.isLower) => Small(name)
    }
  }
}

case class Connection(leftCave: Cave, rightCave: Cave)

class CaveSystem(caves: Set[Cave], connections: Seq[Connection]) {
  val startCave = caves.find(_.isStart).get
  val endCave = caves.find(_.isEnd).get
  val connectionMap = {
    val connectionPairs = connections.flatMap { case Connection(leftCave, rightCave) =>
      Seq(
        leftCave -> rightCave,
        rightCave -> leftCave
      )
    }
    val connectionMap = connectionPairs.groupBy(_._1).mapValues(_.map(_._2))

    connectionMap
  }

  def countPaths: Int = {

    def loop(caves: List[Cave], smallVisited: Set[Cave]): Int = {
      val cave = caves.head
      val count =
          if (cave == endCave) { println(caves); 1 }
          else {
            val nextCaves = connectionMap(cave).filterNot(smallVisited)
            val count = nextCaves.map { nextCave =>
              if (!nextCave.canRevisit) loop(nextCave :: caves, smallVisited + nextCave)
              else loop(nextCave :: caves, smallVisited)
            }.sum

            count
          }

      count
    }

    val count = loop(List(startCave), Set(startCave))

    count
  }
}

object Part1 extends Aoc[Int] {

  def getCavesConnections(lines: Iterator[String]): (Set[Cave], Seq[Connection]) = {
    val caves = MutableSet.empty[Cave]
    val connections: ArrayBuffer[Connection] = ArrayBuffer.empty

    lines.foreach { line =>
      val Array(leftName, rightName) = line.split('-')
      val leftCave = Cave(leftName)
      val rightCave = Cave(rightName)

      caves += leftCave
      caves += rightCave
      connections += Connection(leftCave, rightCave)
    }

    (caves.toSet, connections)
  }

  def mkCaveSystem(lines: Iterator[String]): CaveSystem = {
    val (caves, connections) = getCavesConnections(lines)

    new CaveSystem(caves, connections)
  }

  def run(lines: Iterator[String]): Int = {
    val caveSystem = mkCaveSystem(lines)
    val pathCount = caveSystem.countPaths

    pathCount
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day12/input.txt")

  println(result)
}

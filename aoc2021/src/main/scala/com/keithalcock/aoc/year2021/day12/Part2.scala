package com.keithalcock.aoc.year2021.day12

import com.keithalcock.aoc.year2021.Aoc

class CaveSystem2(caves: Set[Cave], connections: Seq[Connection]) extends CaveSystem(caves, connections) {

  override def countPaths: Int = {

    def loop(caves: List[Cave], smallVisited: Set[Cave], hasWildcard: Boolean): Int = {
      val cave = caves.head
      val count =
          if (cave == endCave) { println(caves); 1 }
          else {
            val nextCaves = connectionMap(cave).filter { nextCave =>
              !smallVisited(nextCave) || (hasWildcard && !nextCave.isStart && !nextCave.isEnd)
            }
            val count = nextCaves.map { nextCave =>
              if (!smallVisited(nextCave))
                if (!nextCave.canRevisit) loop(nextCave :: caves, smallVisited + nextCave, hasWildcard)
                else loop(nextCave :: caves, smallVisited, hasWildcard)
              else
                loop(nextCave :: caves, smallVisited, false)
            }.sum

            count
          }

      count
    }

    val count = loop(List(startCave), Set(startCave), true)

    count
  }
}

object Part2 extends Aoc[Int] {

  def mkCaveSystem2(lines: Iterator[String]): CaveSystem2 = {
    val (caves, connections) = Part1.getCavesConnections(lines)

    new CaveSystem2(caves, connections)
  }

  def run(lines: Iterator[String]): Int = {
    val caveSystem = mkCaveSystem2(lines)
    val pathCount = caveSystem.countPaths

    pathCount
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day12/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2021.day15

import com.keithalcock.aoc.year2021.Aoc

case class Point(x: Int, y: Int)

object Part1 extends Aoc[Int] {

  def cheapestCost(array: Array[Array[Int]]): Int = {
    val width = array.head.length
    val length = array.length
    val costs = Array.fill(length, width)(0)

    def costAt(x: Int, y: Int): Int = {
      if (x == -1 && y == 0) 0
      else if (x == 0 && y == -1) 0
      else if (x < 0) Int.MaxValue
      else if (y < 0) Int.MaxValue
      else costs(y)(x)
    }

    Range(0, length).foreach { y =>
      Range(0, width).foreach { x =>
        costs(y)(x) = array(y)(x) + math.min(costAt(x, y - 1), costAt(x - 1, y))
      }
    }

    costs(length - 1)(width - 1) - costs.head.head
  }

  def score(path: Seq[Point], array: Array[Array[Int]]): Int = {
    path.tail.map { point => array(point.y)(point.x) }.sum
  }

  def run(lines: Iterator[String]): Int = {
    val array = lines.map { line =>
      line.toSeq.map(_.toString.toInt).toArray
    }.toArray
    val width = array(0).length

    array.foreach { vector =>
      require(vector.length == width)
    }

    val cost = cheapestCost(array)

    cost
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day15/input.txt")

  println(result)
}

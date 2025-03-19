package com.keithalcock.aoc.year2021.day13

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Int] {

  def run(lines: Iterator[String]): Int = {
    val (points, folds) = Part1.getPointsFolds(lines)
    val newPoints = folds.foldLeft(points) { case (points, fold) =>
      points.map(fold(_)).distinct
    }
    val maxX = newPoints.maxBy(_.x).x
    val maxY = newPoints.maxBy(_.y).y
    val array = Array.fill[Char](maxY + 1, maxX + 1)('.')

    newPoints.foreach { point =>
      array(point.y)(point.x) = '#'
    }
    array.foreach { line =>
      line.zipWithIndex.foreach { case (letter, index) =>
        if (index % 5 == 0) print(' ')
        print(letter)
      }
      println
    }


    newPoints.length
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day13/input.txt")

  println(result)
}

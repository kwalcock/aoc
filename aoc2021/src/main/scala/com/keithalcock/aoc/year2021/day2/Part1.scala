package com.keithalcock.aoc.year2021.day2

import com.keithalcock.aoc.year2021.Aoc

object Part1 extends Aoc[Int] {

  def run(lines: Iterator[String]): Int = {
    val commands = lines
        .map { command =>
          val Array(direction, distance) = command.split(' ')

          (direction, distance.toInt)
        }
    val (forward, depth) = commands.foldLeft(0, 0) { case ((forward, depth), (direction, distance)) =>
      direction match {
        case "forward" => (forward + distance, depth)
        case "up" => (forward, depth - distance)
        case "down" => (forward, depth + distance)
      }
    }
    val product = forward * depth

    product
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day2/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2021.day2

import scala.io.Source

object Part2 {

  def run(resourceName: String): Int = {
    val commands = Source
        .fromResource(resourceName)
        .getLines
        .map { command =>
          val Array(direction, distance) = command.split(' ')

          (direction, distance.toInt)
        }
    val (forward, depth, aim) = commands.foldLeft(0, 0, 0) { case ((forward, depth, aim), (direction, distance)) =>
      direction match {
        case "forward" => (forward + distance, depth + aim * distance, aim)
        case "up" => (forward, depth, aim - distance)
        case "down" => (forward, depth, aim + distance)
      }
    }
    val product = forward * depth

    product
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day2/input.txt")

  println(result)
}

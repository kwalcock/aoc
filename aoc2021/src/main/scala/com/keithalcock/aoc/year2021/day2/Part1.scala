package com.keithalcock.aoc.year2021.day2

import scala.io.Source

object Part1 extends App {
  val commands = Source
      .fromResource("com/keithalcock/aoc/year2021/day2/input.txt")
      .getLines
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


  println(product)
}

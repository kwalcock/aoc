package com.keithalcock.aoc.year2021.day8

import com.keithalcock.aoc.year2021.Aoc

object Part2 extends Aoc[Int] {
  val alphabet = "abcdefg"
  val displays = List(
    "abcefg", // 0, 6 segments
    "cf", // 1, 2 segments, unique
    "acdeg", // 2, 5 segments
    "acdfg", // 3, 5 segments
    "bcdf", // 4, 4 segments, unique
    "abdfg", // 5, 5 segments
    "abdefg", // 6, 6 segments
    "acf", // 7, 3 segments, unique
    "abcdefg", // 8, 7 segments, unique
    "abcdfg" // 9, 6 segments
  )

  def solve(inputs: Seq[String], outputs: Seq[String]): Int = {
    val permutation = alphabet.permutations.find { permutation =>
      val map = alphabet.zip(permutation).toMap
      val permutatedInputs = inputs.map { input =>
        input.map(map).sorted
      }

      permutatedInputs.forall(displays.contains)
    }
    val map = alphabet.zip(permutation.get).toMap
    val value = outputs.map { output =>
      val permutatedOutput = output.map(map).sorted
      val index = displays.indexOf(permutatedOutput)

      ('0' + index).toChar
    }.mkString.toInt

    value
  }

  def sumSolutions(lines: Seq[String]): Int = {
    val solutions = lines.map { line =>
      val Array(input, output) = line.split('|')
      val inputs = input.split(' ').filterNot(_.isEmpty)
      val outputs = output.split(' ').filterNot(_.isEmpty)
      val solution = solve(inputs, outputs)

      solution
    }

    solutions.sum
  }

  def run(lines: Iterator[String]): Int = {
    val linesList = lines.toList
    val sum = sumSolutions(linesList)

    sum
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day8/input.txt")

  println(result)
}

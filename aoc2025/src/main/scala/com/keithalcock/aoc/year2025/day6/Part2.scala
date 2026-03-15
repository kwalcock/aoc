package com.keithalcock.aoc.year2025.day6

import com.keithalcock.aoc.year2025.Aoc

import scala.collection.mutable.ArrayBuffer

object Part2 extends Aoc[Long]:

  def transpose(lines: Iterator[String]): IndexedSeq[String] =
    val raggedIndexedLines = lines.toIndexedSeq
    val maxWidth = raggedIndexedLines.map(_.length).max
    val indexedLines = raggedIndexedLines.map: line =>
      line + (" " * (maxWidth - line.length))
    val widthRange = indexedLines.head.indices
    val lengthRange = indexedLines.indices
    val transposed = Array.tabulate(widthRange.length): widthIndex =>
      val chars = Array.tabulate(lengthRange.length): lengthIndex =>
        indexedLines(lengthIndex)(widthIndex)

      String(chars)

    transposed.reverse

  def run(lines: Iterator[String]): Long =
    val problems = transpose(lines).iterator
    val results = new ArrayBuffer[Long]()

    while problems.hasNext do
      val strings = problems.takeWhile(_.trim.nonEmpty).toSeq
      val initValues = strings.init.map(_.trim.toLong)
      val lastValue = strings.last.init.trim.toLong
      val values = initValues :+ lastValue
      val operation = strings.last.last
      val result = operation match
        case '*' => values.reduce(_ * _)
        case '+' => values.reduce(_ + _)

      results.append(result)

    val result = results.sum

    result

object Part2App extends App:
  val result = Part2.run("com/keithalcock/aoc/year2025/day6/input.txt")

  println(result)

package com.keithalcock.aoc.year2021.day14

import com.keithalcock.aoc.year2021.Aoc

class Polymer2(val singleCounts: Map[String, Long], val doubleCounts: Map[String, Long], rules: Seq[Rule]) {

  def next: Polymer2 = {
    val doubleAndSingleCounts = rules.map { rule =>
      val count = doubleCounts(rule.left)
      val leftDoubleCount = (rule.left.head.toString + rule.right) -> count
      val midDoubleCount = rule.left -> -count
      val rightDoubleCount = (rule.right + rule.left.last) -> count
      val singleCount = rule.right -> count

      (Seq(leftDoubleCount, midDoubleCount, rightDoubleCount), singleCount)
    }
    val moreDoubleCounts = doubleAndSingleCounts.flatMap(_._1)
    val moreSingleCounts = doubleAndSingleCounts.map(_._2)
    val newDoubleCounts = doubleCounts.map { case (key, value) => key -> (value + moreDoubleCounts.filter(_._1 == key).map(_._2).sum) }
    val newSingleCounts = singleCounts.map { case (key, value) => key -> (value + moreSingleCounts.filter(_._1 == key).map(_._2).sum) }

    new Polymer2(newSingleCounts, newDoubleCounts, rules)
  }

  def score: Long = {
    val max = singleCounts.values.max
    val min = singleCounts.values.min

    max - min
  }
}

object Polymer2 {

  def apply(template: String, rules: Seq[Rule]): Polymer2 = {
    val alphabet = (template.toSeq ++ rules.flatMap { rule =>
      rule.left.toSeq ++ rule.right.toSeq
    }).distinct
    val doubleCounts = alphabet.flatMap { left =>
      alphabet.map { right =>
        s"$left$right" -> template.sliding(2).count { slice => slice.head == left && slice.last == right }.toLong
      }
    }.toMap
    val singleCounts = alphabet.map { letter =>
      letter.toString -> template.count(_ == letter).toLong
    }.toMap

    new Polymer2(singleCounts, doubleCounts, rules)
  }
}

object Part2 extends Aoc[Long] {

  def run(lines: Iterator[String]): Long = {
    val (template, rules) = Part1.getTemplateRules(lines)
    val polymer = Polymer2(template, rules)

    @annotation.tailrec
    def loop(polymer: Polymer2, n: Int): Polymer2 = {
      if (n == 0) polymer
      else loop(polymer.next, n - 1)
    }

    val newPolymer = loop(polymer, 40)

    newPolymer.score
  }
}

object Part2App extends App {
  val result = Part2.run("com/keithalcock/aoc/year2021/day14/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2021.day14

import com.keithalcock.aoc.year2021.Aoc

case class Rule(left: String, right: String)

class Polymer(val values: String, val rules: Seq[Rule]) {
  val map: Map[String, String] = rules.map { rule =>
    rule.left -> rule.right
  }.toMap

  def next: Polymer = {
    val pairs = values.sliding(2)
    val insertions = pairs.map { pair =>
      map.getOrElse(pair, "")
    }
    val stringBuffer = new StringBuffer()

    values.sliding(1).zip(insertions).foreach { case (value, insertion) =>
      stringBuffer.append(value)
      stringBuffer.append(insertion)
    }
    stringBuffer.append(values.last)
    new Polymer(stringBuffer.toString, rules)
  }

  def score: Int = {
    val keys = values.distinct
    val counts = keys.map { key =>
      values.count(_ == key)
    }
    val max = counts.max
    val min = counts.min

    max - min
  }
}

object Part1 extends Aoc[Int] {

  def getTemplateRules(lines: Iterator[String]): (String, List[Rule]) = {
    val template = lines.next
    lines.next
    val rules = lines.map { line =>
      val Array(left, right) = line.split("->").map(_.trim)

      require(left.length == 2)
      require(right.length == 1)
      Rule(left, right)
    }.toList

    (template, rules)
  }

  def run(lines: Iterator[String]): Int = {
    val (template, rules) = getTemplateRules(lines)
    val polymer = new Polymer(template, rules)

    @annotation.tailrec
    def loop(polymer: Polymer, n: Int): Polymer = {
      if (n == 0) polymer
      else loop(polymer.next, n - 1)
    }

    val newPolymer = loop(polymer, 10)

    newPolymer.score
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day14/input.txt")

  println(result)
}

package com.keithalcock.aoc.year2021

import scala.io.Source
import scala.util.Using

abstract class Aoc[T] {

  def run(lines: Iterator[String]): T

  def run(source: Source): T = run(source.getLines)

  def run(resourceName: String): T = {
    Using.resource(Source.fromResource(resourceName)) { source =>
      run(source)
    }
  }
}

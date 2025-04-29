package com.keithalcock.aoc.year2021.day25

import com.keithalcock.aoc.year2021.Aoc

class CucumberMap(val array: Array[Array[Char]]) {
  val length = array.length
  val width = array.head.length

  def next: CucumberMap = {
    var oldArray = array
    val eastArray = Array.fill(length, width)('.')

    0.until(length).foreach { y =>
      0.until(width).foreach { x =>
        if (oldArray(y)(x) == 'v')
          eastArray(y)(x) = oldArray(y)(x)
        else if (oldArray(y)(x) == '>')
          if (oldArray(y)((x + 1) % width) == '.') {
            eastArray(y)(x) = '.'
            eastArray(y)((x + 1) % width) = '>'
          }
          else
            eastArray(y)(x) = oldArray(y)(x)
      }
    }

    oldArray = eastArray

    val southArray = Array.fill(length, width)('.')

    0.until(length).foreach { y =>
      0.until(width).foreach { x =>
        if (oldArray(y)(x) == '>')
          southArray(y)(x) = oldArray(y)(x)
        else if (oldArray(y)(x) == 'v')
          if (oldArray((y + 1) % length)(x) == '.') {
            southArray(y)(x) = '.'
            southArray((y + 1) % length)(x) = 'v'
          }
          else
            southArray(y)(x) = oldArray(y)(x)
      }
    }

    new CucumberMap(southArray)
  }

  override def hashCode: Int = 0 // Always force comparison.

  override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[CucumberMap]
    var same = true

    0.until(length).foreach { y =>
      val thisArray = array(y)
      val thatArray = that.array(y)

      if (same) {
        0.until(width).foreach { x =>
          same = same & thisArray(x) == thatArray(x)
        }
      }
    }
    same
  }
}

object Part1 extends Aoc[Long] {

  def run(lines: Iterator[String]): Long = {
    val stringArray = lines.toArray
    val array = Array.tabulate(stringArray.length, stringArray.head.length) { (y, x) =>
      stringArray(y)(x)
    }
    var cucumberMap = new CucumberMap(array)
    var count = 0

    while ({
      count += 1
      val nextCucumberMap = cucumberMap.next
      val different = nextCucumberMap != cucumberMap

      cucumberMap = nextCucumberMap
      different
    }) { }

    count
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day25/input.txt")

  println(result)
}

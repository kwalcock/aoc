package com.keithalcock.aoc.year2021.day18

import com.keithalcock.aoc.year2021.Aoc

trait Snailfish {
  var parentOpt: Option[DoubleSnailfish] = None

  def explode(): Boolean
  def split(): Boolean
  def toStringBuffer(stringBuffer: StringBuffer): Unit
  def addLeft(child: Snailfish, value: Int): Boolean
  def addRight(child: Snailfish, value: Int): Boolean
  def addLeftRight(value: Int): Boolean
  def addRightLeft(value: Int): Boolean

  def magnitude: Int

  def depth: Int = parentOpt.map(_.depth).getOrElse(-1) + 1

  def setParent(doubleSnailfish: DoubleSnailfish): Unit = parentOpt = Some(doubleSnailfish)
}

class SingleSnailfish(var value: Int) extends Snailfish {

  def magnitude: Int = value

  def toStringBuffer(stringBuffer: StringBuffer): Unit = {
    stringBuffer.append(value.toString)
  }

  override def toString: String = value.toString

  def explode: Boolean = false

  def addValue(value: Int): Boolean = {
    this.value += value
    true
  }

  def addLeft(child: Snailfish, value: Int): Boolean = ???

  def addRight(child: Snailfish, value: Int): Boolean = ???

  def addLeftRight(value: Int): Boolean = {
    addValue(value)
    true
  }

  def addRightLeft(value: Int): Boolean = {
    addValue(value)
    true
  }

  def split: Boolean = false
}

class DoubleSnailfish(var left: Snailfish, var right: Snailfish) extends Snailfish {

  def magnitude: Int = {
    val result = 3 * left.magnitude + 2 * right.magnitude

    result
  }

  def toStringBuffer(stringBuffer: StringBuffer): Unit = {
    stringBuffer.append(DoubleSnailfish.leftSep)
    left.toStringBuffer(stringBuffer)
    stringBuffer.append(DoubleSnailfish.middleSep)
    right.toStringBuffer(stringBuffer)
    stringBuffer.append(DoubleSnailfish.rightSep)
  }

  override def toString: String = {
    val stringBuffer = new StringBuffer()

    toStringBuffer(stringBuffer)
    stringBuffer.toString
  }

  def addLeftRight(value: Int): Boolean = {
    left.addLeftRight(value) || right.addLeftRight(value)
  }

  def addRightLeft(value: Int): Boolean = {
    right.addRightLeft(value) || left.addRightLeft(value)
  }

  def explode(doubleSnailfish: DoubleSnailfish): SingleSnailfish = {
    assert(doubleSnailfish.left.isInstanceOf[SingleSnailfish])
    assert(doubleSnailfish.right.isInstanceOf[SingleSnailfish])

    val singleSnailfish = new SingleSnailfish(0)

    singleSnailfish.setParent(this)
    singleSnailfish
  }

  def addLeft(child: Snailfish, value: Int): Boolean = {
    val done1 = false
    val done2 = done1 || (!left.eq(child) && left.addRightLeft(value))
    val done3 = done2 || parentOpt.exists { parent => parent.addLeft(this, value) }

    done3
  }

  def addRight(child: Snailfish, value: Int): Boolean = {
    val done1 = false
    val done2 = done1 || (!right.eq(child) && right.addLeftRight(value))
    val done3 = done2 || parentOpt.exists { parent => parent.addRight(this, value) }

    done3
  }

  def explode(): Boolean = {
    if (depth < 3)
      left.explode || right.explode
    else {
      if (left.isInstanceOf[DoubleSnailfish]) {
        val doubleSnailfish = left.asInstanceOf[DoubleSnailfish]
        val newLeft = explode(doubleSnailfish)
        val leftValue = doubleSnailfish.left.asInstanceOf[SingleSnailfish].value
        val rightValue = doubleSnailfish.right.asInstanceOf[SingleSnailfish].value

        addLeft(doubleSnailfish, leftValue)
        addRight(doubleSnailfish, rightValue)
        left = newLeft
        true
      }
      else if (right.isInstanceOf[DoubleSnailfish]) {
        val doubleSnailfish = right.asInstanceOf[DoubleSnailfish]
        val newRight = explode(doubleSnailfish)
        val leftValue = doubleSnailfish.left.asInstanceOf[SingleSnailfish].value
        val rightValue = doubleSnailfish.right.asInstanceOf[SingleSnailfish].value

        addLeft(doubleSnailfish, leftValue)
        addRight(doubleSnailfish, rightValue)
        right = newRight
        true
      }
      else false
    }
  }

  def newDoubleSnailfish(singleSnailfish: SingleSnailfish): DoubleSnailfish = {
    val leftValue = singleSnailfish.value / 2
    val rightValue = (singleSnailfish.value + 1) / 2
    val leftSingleSnailfish = new SingleSnailfish(leftValue)
    val rightSingleSnailfish = new SingleSnailfish(rightValue)
    val doubleSnailfish = new DoubleSnailfish(new SingleSnailfish(leftValue), new SingleSnailfish(rightValue))

    leftSingleSnailfish.setParent(doubleSnailfish)
    rightSingleSnailfish.setParent(doubleSnailfish)
    doubleSnailfish.setParent(this)
    doubleSnailfish
  }

  def split: Boolean = {
    val done1 = false
    val done2 = done1 || {
      if (left.isInstanceOf[SingleSnailfish]) {
        val singleSnailfish = left.asInstanceOf[SingleSnailfish]

        if (singleSnailfish.value >= 10) {
          left = newDoubleSnailfish(singleSnailfish)
          true
        }
        else false
      }
      else false
    } || left.split
    val done3 = done2 || {
      if (right.isInstanceOf[SingleSnailfish]) {
        val singleSnailfish = right.asInstanceOf[SingleSnailfish]

        if (singleSnailfish.value >= 10) {
          right = newDoubleSnailfish(singleSnailfish)
          true
        }
        else false
      }
      else false
    } || right.split

    done3
  }
}

object DoubleSnailfish {
  val leftSep = '['
  val middleSep = ','
  val rightSep = ']'

  def newSingleOrDoubleSnailfish(string: String): Snailfish = {
    if (string.head == leftSep) apply(string)
    else new SingleSnailfish(Integer.parseInt(string))
  }

  def apply(string: String): DoubleSnailfish = {
    assert(string.head == leftSep)
    assert(string.last == rightSep)

    var depth = 0
    val depths = string.map { char =>
      if (char == leftSep) {
        depth += 1
        depth
      }
      else if (char == rightSep) {
        val oldDepth = depth

        depth -= 1
        oldDepth
      }
      else depth
    }
    val middleIndex = string.indices.find { index =>
      string(index) == middleSep && depths(index) == 1
    }.get
    val leftString = string.slice(1, middleIndex)
    val rightString = string.slice(middleIndex + 1, string.length - 1)
    val leftSnailfish = newSingleOrDoubleSnailfish(leftString)
    val rightSnailfish = newSingleOrDoubleSnailfish(rightString)
    val result = new DoubleSnailfish(leftSnailfish, rightSnailfish)

    leftSnailfish.setParent(result)
    rightSnailfish.setParent(result)
    result
  }

  def reduce(doubleSnailfish: DoubleSnailfish): Unit = {
    while (
      doubleSnailfish.explode() || doubleSnailfish.split()
    ) { }
  }

  def add(leftDoubleSnailfish: DoubleSnailfish, rightDoubleSnailfish: DoubleSnailfish): DoubleSnailfish = {
    val sum = new DoubleSnailfish(leftDoubleSnailfish, rightDoubleSnailfish)

    leftDoubleSnailfish.setParent(sum)
    rightDoubleSnailfish.setParent(sum)
    reduce(sum)
    sum
  }
}

object Part1 extends Aoc[Int] {

  def sum(doubleSnailfishes: Seq[DoubleSnailfish]): DoubleSnailfish = {
    val result = doubleSnailfishes.reduceLeft { (left, right) =>
      val next = DoubleSnailfish.add(left, right)

      next
    }

    result
  }

  def runString(string: String): DoubleSnailfish = {
    val result = DoubleSnailfish(string)

    result
  }

  def run(lines: Iterator[String]): Int = {
    val snailfishes = lines.map(runString).toArray
    val magnitude = sum(snailfishes).magnitude

    magnitude
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day18/input.txt")

  println(result)
}

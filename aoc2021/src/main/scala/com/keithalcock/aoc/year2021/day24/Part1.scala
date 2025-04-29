package com.keithalcock.aoc.year2021.day24

import com.keithalcock.aoc.year2021.Aoc

import scala.collection.mutable.{Map => MutableMap}

class State(regs: Array[Long]) {

  def set(reg: Int, value: Long): Unit = regs(reg) = value

  def get(reg: Int): Long = regs(reg)

  def reset(): Unit = {
    regs(0) = 0L
    regs(1) = 0L
    regs(2) = 0L
    regs(3) = 0L
  }
}

class Input(input: Array[Int]) {
  var pos = 0

  def get: Int = {
    val result = input(pos)

    pos += 1
    result
  }
}

trait Command {
  def execute(state: State, input: Input): Unit
}

object Command {
  val    inpPattern = "^inp ([wxyz])$".r

  val addRegPattern = "^add ([wxyz]) ([wxyz])$".r
  val mulRegPattern = "^mul ([wxyz]) ([wxyz])$".r
  val divRegPattern = "^div ([wxyz]) ([wxyz])$".r
  val modRegPattern = "^mod ([wxyz]) ([wxyz])$".r
  val eqlRegPattern = "^eql ([wxyz]) ([wxyz])$".r

  val addValPattern = "^add ([wxyz]) (-?\\d+)$".r
  val mulValPattern = "^mul ([wxyz]) (-?\\d+)$".r
  val divValPattern = "^div ([wxyz]) (-?\\d+)$".r
  val modValPattern = "^mod ([wxyz]) (-?\\d+)$".r
  val eqlValPattern = "^eql ([wxyz]) (-?\\d+)$".r

  def regIndex(reg: String): Int = {
    reg.head match {
      case 'w' => 0
      case 'x' => 1
      case 'y' => 2
      case 'z' => 3
    }
  }

  def regValue(value: String): Int = {
    value.toInt
  }

  def apply(line: String): Command = {
    line match {
      case inpPattern(reg) => Inp(regIndex(reg))

      case addRegPattern(left, right) => AddReg(regIndex(left), regIndex(right))
      case mulRegPattern(left, right) => MulReg(regIndex(left), regIndex(right))
      case divRegPattern(left, right) => DivReg(regIndex(left), regIndex(right))
      case modRegPattern(left, right) => ModReg(regIndex(left), regIndex(right))
      case eqlRegPattern(left, right) => EqlReg(regIndex(left), regIndex(right))

      case addValPattern(left, right) => AddVal(regIndex(left), regValue(right))
      case mulValPattern(left, right) => MulVal(regIndex(left), regValue(right))
      case divValPattern(left, right) => DivVal(regIndex(left), regValue(right))
      case modValPattern(left, right) => ModVal(regIndex(left), regValue(right))
      case eqlValPattern(left, right) => EqlVal(regIndex(left), regValue(right))
    }
  }
}

case class Inp(reg: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    val value = input.get

    state.set(reg, value)
  }
}

case class AddReg(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) + state.get(right))
  }
}

case class AddVal(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) + right)
  }
}

case class MulReg(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) * state.get(right))
  }
}

case class MulVal(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) * right)
  }
}

case class DivReg(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) / state.get(right))
  }
}

case class DivVal(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) / right)
  }
}

case class ModReg(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) % state.get(right))
  }
}

case class ModVal(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, state.get(left) % right)
  }
}

case class EqlReg(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, if (state.get(left) == state.get(right)) 1 else 0)
  }
}

case class EqlVal(left: Int, right: Int) extends Command {
  def execute(state: State, input: Input): Unit = {
    state.set(left, if (state.get(left) == right) 1 else 0)
  }
}

class Machine(commands: Seq[Command]) {
  val state = new State(Array(0, 0, 0, 0))

  def process(input: Input, z: Long): Long = {
    state.reset()
    state.set(3, z)

    commands.foreach { command =>
      command.execute(state, input)
    }

    state.get(3)
  }
}

object Part1 extends Aoc[Long] {

  def run(lines: Iterator[String]): Long = {
    val line = lines.mkString("\n")
    val stanzas = line.split("\n\n")
    val machines = stanzas.map { stanza =>
      val commands = stanza.split('\n').map { line =>
        Command(line)
      }

      new Machine(commands)
    }
    val inputArray = Array(0)
    var zMap = MutableMap(0L -> 0L) // z -> input

    machines.foreach { machine =>
      val newZMap = MutableMap[Long, Long]()

      Range.inclusive(1, 9).foreach { w =>
        zMap.foreach { case (oldZ, oldInput) =>
          inputArray(0) = w
          val newZ = machine.process(new Input(inputArray), oldZ)
          val newInput = oldInput * 10 + w
          val prevInputOpt = newZMap.get(newZ)

          if (prevInputOpt.isEmpty || newInput > prevInputOpt.get)
            newZMap(newZ) = newInput
        }
      }
      zMap = newZMap
    }

    val maxZeroInput = zMap(0L)

    maxZeroInput
  }
}

object Part1App extends App {
  val result = Part1.run("com/keithalcock/aoc/year2021/day24/input.txt")

  println(result)
}

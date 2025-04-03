package com.keithalcock.aoc.year2021.day17

object Part2 {

  def run(targetX: Range, targetY: Range): Int = {
    val xStartVelocities = Range.inclusive(0, targetX.max).filter { xStartVelocity =>
      var xPosition = 0
      val xPositions = Range.inclusive(xStartVelocity, 1, -1).map { xVelocity =>
        xPosition += xVelocity
        xPosition
      }
      val bullseye = xPositions.exists(targetX.contains)

      bullseye
    }
    val velocityPairs = xStartVelocities.flatMap { xStartVelocity =>
      // We could just go until some value misses and assume that no others hit.
      val yStartVelocities = Range(targetY.min, 100).flatMap { yStartVelocity =>
        val yMaxOpt = Part1.run(targetX, targetY, xStartVelocity, yStartVelocity)

        yMaxOpt.map(_ => yStartVelocity)
      }

      yStartVelocities.map { yStartVelocity =>
        (xStartVelocity, yStartVelocity)
      }
    }

    velocityPairs.length
  }
}

object Part2App extends App {
  // val result = Part2.run(Range.inclusive(20, 30), Range.inclusive(-10, -5))
  val result = Part2.run(Range.inclusive(282, 314), Range.inclusive(-80, -45))

  println(result)
}

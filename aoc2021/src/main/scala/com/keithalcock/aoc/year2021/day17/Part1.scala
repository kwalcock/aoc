package com.keithalcock.aoc.year2021.day17

import com.keithalcock.aoc.year2021.utils.Point

case class MovingPoint(point: Point, xVelocity: Int, yVelocity: Int) {

  def next: MovingPoint = MovingPoint(
    Point(point.x + xVelocity, point.y + yVelocity),
    xVelocity - math.signum(xVelocity),
    yVelocity - 1
  )
}

class MovingPointIterator(targetX: Range, targetY: Range, movingPoint: MovingPoint) extends Iterator[Point] {
  var currentMovingPoint = movingPoint

  override def hasNext: Boolean = {
    val nextMovingPoint = currentMovingPoint.next

    (nextMovingPoint.point.x <= targetX.max) && (targetY.min <= nextMovingPoint.point.y)
  }

  override def next(): Point = {
    currentMovingPoint = currentMovingPoint.next
    currentMovingPoint.point
  }
}

object Part1 {

  def run(targetX: Range, targetY: Range, xVelocity: Int, yVelocity: Int): Option[Int] = {
    val movingPoint = MovingPoint(Point(0, 0), xVelocity, yVelocity)
    val iterator = new MovingPointIterator(targetX, targetY, movingPoint)
    val points = iterator.toArray
    val bullseye = points.exists { point =>
      targetX.contains(point.x) && targetY.contains(point.y)
    }

    if (bullseye) Some(points.maxBy(_.y).y)
    else None
  }

  def run(targetX: Range, targetY: Range): Int = {
    val xStartVelocities = Range.inclusive(0, targetX.max).filter { xStartVelocity =>
      var xPosition = 0
      val xPositions = Range.inclusive(xStartVelocity, 1, -1).map { xVelocity =>
        xPosition += xVelocity
        xPosition
      }
      val bullseye = xPositions.exists(targetX.contains)
      val atZero = xPositions.nonEmpty && targetX.contains(xPositions.last)

      if (atZero)
        println("There is a problem.") // This happens at 24
      atZero
    }
    val velocityPairs = xStartVelocities.flatMap { xStartVelocity =>
      // We could just go until some value misses and assume that no others hit.
      val yStartVelocities = Range(0, 100).flatMap { yStartVelocity =>
        val yMaxOpt = run(targetX, targetY, xStartVelocity, yStartVelocity)

        yMaxOpt.map(_ => yStartVelocity)
      }

      yStartVelocities.map { yStartVelocity =>
        (xStartVelocity, yStartVelocity)
      }
    }
    val heights = velocityPairs.flatMap { case (xStartVelocity, yStartVelocity) =>
      run(targetX, targetY, xStartVelocity, yStartVelocity)
    }
    val maxHeight = heights.max

    maxHeight
  }
}

object Part1App extends App {
  // val result = Part1.run(Range.inclusive(20, 30), Range.inclusive(-10, -5))
  val result = Part1.run(Range.inclusive(282, 314), Range.inclusive(-80, -45))

  println(result)
}

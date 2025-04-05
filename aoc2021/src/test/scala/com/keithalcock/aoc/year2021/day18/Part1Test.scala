package com.keithalcock.aoc.year2021.day18

import com.keithalcock.aoc.Test

class Part1Test extends Test {

  behavior of "Part1"

  it should "parse Snailfish numbers" in {
    {
      val expectedResult = "[1,2]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[1,2],3]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[9,[8,7]]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[1,9],[8,5]]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
      val actualResult = DoubleSnailfish(expectedResult).toString

      actualResult should be (expectedResult)
    }
  }

  it should "calculate magnitudes" in {
    {
      val expectedResult = 143
      val actualResult = DoubleSnailfish("[[1,2],[[3,4],5]]").magnitude

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = 1384
      val actualResult = DoubleSnailfish("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = 445
      val actualResult = DoubleSnailfish("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = 791
      val actualResult = DoubleSnailfish("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = 1137
      val actualResult = DoubleSnailfish("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = 3488
      val actualResult = DoubleSnailfish("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude

      actualResult should be (expectedResult)
    }
  }

  it should "explode" in {
    {
      val expectedResult = "[[[[0,9],2],3],4]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[[[[9,8],1],2],3],4]")

        doubleSnailfish.explode()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[7,[6,[5,[7,0]]]]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[7,[6,[5,[4,[3,2]]]]]")

        doubleSnailfish.explode()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[6,[5,[7,0]]],3]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[6,[5,[4,[3,2]]]],1]")

        doubleSnailfish.explode()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")

        doubleSnailfish.explode()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")

        doubleSnailfish.explode()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }
  }

  it should "split Snailfish numbers" in {
    {
      val expectedResult = "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[[[0,7],4],[15,[0,13]]],[1,1]]")

        doubleSnailfish.split()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"
      val actualResult = {
        val doubleSnailfish = DoubleSnailfish("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")

        doubleSnailfish.split()
        doubleSnailfish.toString
      }

      actualResult should be (expectedResult)
    }
  }

  it should "add Snailfish numbers" in {
    {
      val expectedResult = "[[1,2],[[3,4],5]]"
      val actualResult = DoubleSnailfish.add(
        DoubleSnailfish("[1,2]"),
        DoubleSnailfish("[[3,4],5]")
      ).toString

      actualResult should be (expectedResult)
    }
  }

  it should "sum a list of Snailfish numbers" in {
    {
      val expectedResult = "[[[[1,1],[2,2]],[3,3]],[4,4]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[1,1]"),
        DoubleSnailfish("[2,2]"),
        DoubleSnailfish("[3,3]"),
        DoubleSnailfish("[4,4]"),
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[3,0],[5,3]],[4,4]],[5,5]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[1,1]"),
        DoubleSnailfish("[2,2]"),
        DoubleSnailfish("[3,3]"),
        DoubleSnailfish("[4,4]"),
        DoubleSnailfish("[5,5]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[5,0],[7,4]],[5,5]],[6,6]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[1,1]"),
        DoubleSnailfish("[2,2]"),
        DoubleSnailfish("[3,3]"),
        DoubleSnailfish("[4,4]"),
        DoubleSnailfish("[5,5]"),
        DoubleSnailfish("[6,6]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"),
        DoubleSnailfish("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"),
        DoubleSnailfish("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"),
        DoubleSnailfish("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"),
        DoubleSnailfish("[7,[5,[[3,8],[1,4]]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"),
        DoubleSnailfish("[[2,[2,2]],[8,[8,1]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"),
        DoubleSnailfish("[2,9]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"),
        DoubleSnailfish("[1,[[[9,3],9],[[9,0],[0,7]]]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"),
        DoubleSnailfish("[[[5,[7,4]],7],1]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"),
        DoubleSnailfish("[[[[4,2],2],6],[8,7]]")
      )).toString

      actualResult should be (expectedResult)
    }

    {
      val expectedResult = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
      val actualResult = Part1.sum(Seq(
        DoubleSnailfish("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"),
        DoubleSnailfish("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"),
        DoubleSnailfish("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"),
        DoubleSnailfish("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"),
        DoubleSnailfish("[7,[5,[[3,8],[1,4]]]]"),
        DoubleSnailfish("[[2,[2,2]],[8,[8,1]]]"),
        DoubleSnailfish("[2,9]"),
        DoubleSnailfish("[1,[[[9,3],9],[[9,0],[0,7]]]]"),
        DoubleSnailfish("[[[5,[7,4]],7],1]"),
        DoubleSnailfish("[[[[4,2],2],6],[8,7]]")
      )).toString

      actualResult should be (expectedResult)
    }
  }
}

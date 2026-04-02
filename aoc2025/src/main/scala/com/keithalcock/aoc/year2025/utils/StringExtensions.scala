package com.keithalcock.aoc.year2025.utils

trait StringExtensions:

  extension (string: String)

    def isplit(separator: Char): IArray[String] =
      IArray.unsafeFromArray(string.split(separator))

    def isplit(regex: String): IArray[String] =
      IArray.unsafeFromArray(string.split(regex))

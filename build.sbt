val scalaVersion = "2.12.20"

lazy val root = (project in file("."))
  .aggregate(aoc2021)

lazy val aoc2021 = project.in(file("aoc2021"))

val scalaVersion = "2.12.21"

lazy val root = (project in file("."))
  .aggregate(aoc2021, aoc2025)

lazy val aoc2021 = project.in(file("aoc2021"))

lazy val aoc2025 = project.in(file("aoc2025"))

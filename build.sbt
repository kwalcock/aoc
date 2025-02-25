val scala3Version = "3.3.5"

lazy val root = (project in file("."))
  .aggregate(aoc2021)

lazy val aoc2021 = project.in(file("aoc2021"))

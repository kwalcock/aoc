val scala2 = "2.12.21"
val scala3 = "3.3.7"

ThisBuild/crossScalaVersions := Seq(scala2, scala3)
ThisBuild/scalaVersion := scala2

 lazy val root = (project in file("."))
    .aggregate(aoc2021, aoc2025)

lazy val aoc2021 = project.in(file("aoc2021"))
    .settings(
      crossScalaVersions := Seq(scala2),
      scalaVersion := scala2
    )

lazy val aoc2025 = project.in(file("aoc2025"))
    .settings(
      crossScalaVersions := Seq(scala3),
      scalaVersion := scala3
    )

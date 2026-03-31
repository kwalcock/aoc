// val scala2 = "2.12.21" // not good for scala native
val scala2 = "2.12.20"
// val scala3 = "3.3.7" // not good for scala native
val scala3 = "3.3.4"

//ThisBuild/crossScalaVersions := Seq(scala2, scala3)
ThisBuild/scalaVersion := scala3

 lazy val root = (project in file("."))
    .aggregate(aoc2021, aoc2025)
    .settings(
      crossScalaVersions := Nil,
      publish / skip := true
    )

lazy val aoc2021 = project.in(file("aoc2021"))
    .settings(
      crossScalaVersions := Seq(scala2),
      scalaVersion := scala2
    )

lazy val aoc2025 = project.in(file("aoc2025"))
    .enablePlugins(GraalVMNativeImagePlugin)
    .settings(
      crossScalaVersions := Seq(scala3),
      scalaVersion := scala3,
      scalacOptions := Seq("-deprecation")
    )

// lazy val helloworld = crossProject(JVMPlatform, NativePlatform).in(file("helloworld"))
lazy val helloworld = project.in(file("helloworld"))
    .enablePlugins(ScalaNativePlugin)

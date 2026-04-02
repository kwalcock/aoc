addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")

// For native applications.  Versions could be updated for more recent Scala versions.
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.20.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.10")

// This is for building with GraalVM.
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.11.7")

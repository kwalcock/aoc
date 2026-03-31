// Use aoc2025 / GraalVMNativeImage / packageBin to produce the image.
// Find the output in target/graalvm-native-image/aoc2025[.exe].

GraalVMNativeImage / mainClass := Some("com.keithalcock.aoc.year2025.day1.Part1App")

graalVMNativeImageOptions := Seq(
  "--verbose",
  "--allow-incomplete-classpath",
  "--report-unsupported-elements-at-runtime",
  "--no-fallback",
  "-H:IncludeResources=\".*/day1/input\\.txt\""
)

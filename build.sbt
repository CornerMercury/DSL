scalaVersion := "3.6.3"

name := "DSL"
libraryDependencies ++= Seq(
  "com.github.j-mie6" %% "parsley" % "4.6.2",
//   "com.lihaoyi" %% "os-lib" % "0.11.3",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

Compile / scalaSource := baseDirectory.value / "src" / "main" / "DSL"
Test / scalaSource := baseDirectory.value / "src" / "test" / "DSL"
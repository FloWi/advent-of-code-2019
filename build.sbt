name := "advent-of-code"

lazy val `advent-of-code` = (project in file("."))
  .settings(
    libraryDependencies ++= List(
      "co.fs2" %% "fs2-core" % "2.1.0", // For cats 2 and cats-effect 2
      "co.fs2" %% "fs2-io" % "2.1.0",
      "org.scalatest" %% "scalatest" % "3.0.5"
    )
  )

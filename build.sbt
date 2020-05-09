val finchVersion = "0.26.0"
val circeVersion = "0.10.1"
val scalatestVersion = "3.0.5"
val caffeineVersion = "0.28.0"

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.montokapro",
    name := "set-logic-service",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core"  % finchVersion,
      "com.github.finagle" %% "finchx-circe"  % finchVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      "com.github.cb372" %% "scalacache-cats-effect" % caffeineVersion
    )
  )

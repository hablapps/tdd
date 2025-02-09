val scala3Version = "3.6.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "chmacros",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq(
      "-experimental"
    ),
    
    
    libraryDependencies ++= List(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-free" % "2.9.0",
      "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
  )
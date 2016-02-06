name := "HZUtil"

version := "1.6.0"

organization := "org.hirosezouen"

scalaVersion := "2.10.5"

// Actor of Ver2.10.1-> requires to add libraryDependencies explicitly
libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-actors" % _ }

// Reflect of Ver2.10.1-> requires to add libraryDependencies explicitly
libraryDependencies <+= scalaVersion { "org.scala-lang" % "scala-reflect" % _ }

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.4" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"

libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.3"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12"

// Avoid sbt warning ([warn] This usage is deprecated and will be removed in sbt 1.0)
// Current Sbt dose not allow overwrite stabele release created publicLocal task.
isSnapshot := true

parallelExecution in Test := false

scalacOptions += "-deprecation"

scalacOptions += "-feature"

//logLevel := Level.Debug


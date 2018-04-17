
organizationName := "Opalia Systems"

organization := "systems.opalia"

name := "commons"

homepage := Some(url("https://github.com/OpaliaSystems/opalia-commons"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "com.typesafe" % "config" % "1.3.3",
  "com.typesafe.play" %% "play-json" % "2.6.9",
  "org.apache.commons" % "commons-text" % "1.2",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.jfree" % "jfreechart" % "1.5.0" % "man"
)

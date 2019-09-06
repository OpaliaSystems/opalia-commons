
organizationName := "Opalia Systems"

organizationHomepage := Some(url("https://opalia.systems"))

organization := "systems.opalia"

name := "commons"

description := "The project contains common implementations used in Opalia stack."

homepage := Some(url("https://github.com/OpaliaSystems/opalia-commons"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "systems.opalia" %% "interfaces" % "1.0.0",
  "com.typesafe.play" %% "play-json" % "2.7.2",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.61",
  "org.parboiled" %% "parboiled" % "2.1.5",
  "org.apache.commons" % "commons-text" % "1.6",
  "org.graalvm.sdk" % "graal-sdk" % "19.2.0.1" % "provided",
  "org.graalvm.sdk" % "graal-sdk" % "19.2.0.1" % "test",
  "org.graalvm.js" % "js" % "19.2.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.7" % "test",
  "org.jfree" % "jfreechart" % "1.5.0" % "man"
)

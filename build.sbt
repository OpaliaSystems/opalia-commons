
organizationName := "Opalia Systems"

organizationHomepage := Some(url("https://opalia.systems"))

organization := "systems.opalia"

name := "commons"

description := "The project contains common implementations used in Opalia stack."

homepage := Some(url("https://github.com/OpaliaSystems/opalia-commons"))

version := "1.0.0"

scalaVersion := "2.12.13"

libraryDependencies ++= Seq(
  "systems.opalia" %% "interfaces" % "1.0.0",
  "com.typesafe.play" %% "play-json" % "2.9.2",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.68",
  "org.parboiled" %% "parboiled" % "2.2.1",
  "org.apache.commons" % "commons-text" % "1.9",
  "org.graalvm.sdk" % "graal-sdk" % "21.0.0.2" % "provided",
  "org.graalvm.sdk" % "graal-sdk" % "21.0.0.2" % "test",
  "org.graalvm.js" % "js" % "21.0.0.2" % "test",
  "org.scalatest" %% "scalatest" % "3.2.5" % "test",
  "org.jfree" % "jfreechart" % "1.5.3" % "man"
)

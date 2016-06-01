
organizationName := "Opalia Systems"

organization := "systems.opalia"

name := "commons"

homepage := Some(url("https://github.com/OpaliaSystems/opalia-commons"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"


resolvers ++= Seq(
  Resolver.typesafeRepo("maven-releases"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.5.3",
  "joda-time" % "joda-time" % "2.9.4",
  "commons-codec" % "commons-codec" % "1.10",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)

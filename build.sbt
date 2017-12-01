
organizationName := "Opalia Systems"

organization := "systems.opalia"

name := "commons"

homepage := Some(url("https://github.com/OpaliaSystems/opalia-commons"))

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.3"


resolvers ++= Seq(
  Resolver.typesafeRepo("maven-releases"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "com.typesafe" % "config" % "1.3.1",
  "com.typesafe.play" %% "play-json" % "2.6.3",
  "org.apache.commons" % "commons-text" % "1.1",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)


val IntegrationTest = config("it") extend Test
val EndToEndTest = config("e2e") extend Test

val itSettings =
  inConfig(IntegrationTest)(Defaults.testSettings) ++
    Seq(

      resourceDirectory in IntegrationTest := baseDirectory.value / "src/test-it/resources",
      javaSource in IntegrationTest := baseDirectory.value / "src/test-it/java",
      scalaSource in IntegrationTest := baseDirectory.value / "src/test-it/scala",
    )

val e2eSettings =
  inConfig(EndToEndTest)(Defaults.testSettings) ++
    Seq(

      resourceDirectory in EndToEndTest := baseDirectory.value / "src/test-e2e/resources",
      javaSource in EndToEndTest := baseDirectory.value / "src/test-e2e/java",
      scalaSource in EndToEndTest := baseDirectory.value / "src/test-e2e/scala"
    )

lazy val root =
  project.in(file("."))
    .configs(IntegrationTest)
    .configs(EndToEndTest)
    .settings(itSettings, e2eSettings)

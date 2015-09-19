
lazy val conf =
  com.typesafe.config.ConfigFactory.parseFile(new File("./src/main/resources/application.conf")).resolve()

lazy val root =
  (project in file("."))
    .settings(

      scalaVersion := "2.11.7",

      name := conf.getString("project.name"),
      version := conf.getString("project.version"),

      resolvers ++= Seq(
        Resolver.typesafeRepo("maven-releases"),
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
      ),

      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-json" % "2.3.0",
        "joda-time" % "joda-time" % "2.8.2",
        "commons-codec" % "commons-codec" % "1.10",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )

val commonSettings = Seq(
  scalaVersion := "2.12.1"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers, tests)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )
  .dependsOn(tests % "compile->test")

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

lazy val moduleToTest = if(System.getProperty("test") == "answers") answers else exercises

lazy val tests = (project in file("tests"))
  .settings(commonSettings ++ Seq(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "4.7.0" % "test",
      "org.specs2" %% "specs2-junit" % "4.7.0"
    ),
    resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
  ))
  .settings(
    name := "test"
  )
  .dependsOn(moduleToTest).aggregate(moduleToTest)

scalaVersion := "3.8.4"

lazy val root = rootProject
  .settings(
    name := "MonadCore2D",
    libraryDependencies ++= Seq(
      //You can add library dependencies here, for example,
      "org.scalactic" %% "scalactic" % "3.2.20",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      "org.scalamock" %% "scalamock" % "7.5.5" % Test,
      //"org.scalameta" %% "munit" % "1.2.3" % Test
    )
  )

scalaVersion := "3.8.4"

lazy val javaFXVersion = "23.0.1"
lazy val javaFXModules = Seq("base", "controls", "graphics")
// Un solo fat JAR per Windows x64, Linux x64 e macOS ARM.
// JavaFX non permette di includere x64 e ARM dello stesso sistema nello stesso JAR:
// le rispettive librerie native hanno gli stessi nomi di file.
lazy val javaFXClassifiers = Seq("win", "linux", "mac-aarch64")

lazy val root = rootProject
  .settings(
    name := "MonadCore2D",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.20",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      "org.scalamock" %% "scalamock" % "7.5.5" % Test,
      "dev.langchain4j" % "langchain4j-ollama" % "1.17.2",
      "dev.langchain4j" % "langchain4j" % "1.17.2",
      "org.scalafx" %% "scalafx" % "23.0.1-R34"
    ) ++ javaFXModules.flatMap(module => javaFXClassifiers.map(classifier =>
      ("org.openjfx" % s"javafx-$module" % javaFXVersion).classifier(classifier)
    ))
  ).settings(
    assembly / mainClass := Some("Launcher"),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", _*)             => MergeStrategy.discard
      case "module-info.class"                  => MergeStrategy.discard
      case _                                    => MergeStrategy.first
    }
  )

ThisBuild / scalacOptions ++= Seq(
  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s"
)

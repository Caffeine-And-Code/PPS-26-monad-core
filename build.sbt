scalaVersion := "3.8.4"

// Classify the os to choose the building dependencies
lazy val osClassifier: String = {
  val osName = System.getProperty("os.name").toLowerCase
  val osArch = System.getProperty("os.arch").toLowerCase
  val isArm = osArch.contains("aarch64") || osArch.contains("arm")

  if (osName.contains("mac"))
    if (isArm) "mac-aarch64" else "mac"
  else if (osName.contains("win"))
    "win"
  else if (osName.contains("linux"))
    if (isArm) "linux-aarch64" else "linux"
  else
    throw new Exception(s"Piattaforma non supportata: $osName / $osArch")
}

lazy val javaFXVersion = "23.0.1"
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case PathList("META-INF", "substrate", "config", _*) => MergeStrategy.discard
  case path =>
    val previous = (assembly / assemblyMergeStrategy).value
    previous(path)
}

// build dynamically the module-path of javaFx to resolve the warning upon application startup
lazy val javaFXModulePath = Def.task {
  val cp = (Test / dependencyClasspath).value
  val converter = fileConverter.value
  cp.map(af => converter.toPath(af.data))
    .filter(p => p.getFileName.toString.startsWith("javafx-"))
    .map(_.toAbsolutePath.toString)
    .mkString(java.io.File.pathSeparator)
}

lazy val javaFXJavaOptions = Def.task {
  Seq(
    "--module-path", javaFXModulePath.value,
    "--add-modules", javaFXModules.map(m => s"javafx.$m").mkString(",")
  )
}

lazy val LlmIntegrationTest =
  config("llmIntegrationTest").extend(Test)

lazy val root = rootProject
  .configs(LlmIntegrationTest)
  .settings(
    name := "MonadCore2D",
    inConfig(LlmIntegrationTest)(Defaults.testSettings),
    LlmIntegrationTest / scalaSource :=
      baseDirectory.value / "src" / "llmIntegrationTest" / "scala",
    LlmIntegrationTest / resourceDirectory :=
      baseDirectory.value / "src" / "llmIntegrationTest" / "resources",
    LlmIntegrationTest / parallelExecution := false,
    LlmIntegrationTest / fork := true,
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.20",
      "org.scalatest" %% "scalatest" % "3.2.20" % "test,llmIntegrationTest",
      "org.scalamock" %% "scalamock" % "7.5.5" % "test,llmIntegrationTest",
      "org.testfx" % "testfx-core" % "4.0.18" % Test,
      "dev.langchain4j" % "langchain4j-ollama" % "1.17.2",
      "dev.langchain4j" % "langchain4j" % "1.17.2",
      "org.scalafx" %% "scalafx" % "23.0.1-R34"
    ) ++ javaFXModules.map(m =>
      ("org.openjfx" % s"javafx-$m" % javaFXVersion).classifier(osClassifier)
    ),

    // fork tests, a JVM foreach test class
    Test / fork := true,
    Test / testForkedParallel := false,
    Test / javaOptions ++= javaFXJavaOptions.value,

    Test / testGrouping := Def.uncached {
      val jvmOpts = javaFXJavaOptions.value.toVector
      (Test / definedTests).value.map { suite =>
        Tests.Group(
          name = suite.name,
          tests = Seq(suite),
          runPolicy = Tests.SubProcess(
            ForkOptions().withRunJVMOptions(jvmOpts)
          )
        )
      }
    }
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
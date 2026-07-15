scalaVersion := "3.8.4"

// Determina os + architettura per scegliere il classifier JavaFX corretto
lazy val osClassifier: String = {
  val osName = System.getProperty("os.name").toLowerCase
  val osArch = System.getProperty("os.arch").toLowerCase
  val isArm  = osArch.contains("aarch64") || osArch.contains("arm")

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

lazy val IntegrationTests = config("integrationTests").extend(Test)

lazy val root = rootProject
  .configs(IntegrationTests)
  .settings(
    inConfig(IntegrationTests)(Defaults.testSettings),
    name := "MonadCore2D",
    IntegrationTests / scalaSource := baseDirectory.value / "src" / "integrationTests" / "scala",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.20",
      "org.scalatest" %% "scalatest" % "3.2.20" % Test,
      "org.scalamock" %% "scalamock" % "7.5.5" % Test,
      "org.scalafx" %% "scalafx" % "23.0.1-R34"
    ) ++ javaFXModules.map(m =>
      "org.openjfx" % s"javafx-$m" % javaFXVersion classifier osClassifier
    )
  )
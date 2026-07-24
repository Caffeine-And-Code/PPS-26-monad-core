package integrations.monad_core.simulator.presentation.support

import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path, Paths}

trait SnapshotTesting extends Matchers:

  def snapshotsDir: Path = Paths.get("src/test/scala/integrations/monad_core/simulator/snapshots")


  def assertMatchesSnapshot(snapshotName: String, actualContent: String): Unit =
    val fileName = if snapshotName.endsWith(".json") then snapshotName else s"$snapshotName.json"
    val snapshotPath = snapshotsDir.resolve(fileName)
    val snapshotFile = snapshotPath.toFile

    val normalizedActual = actualContent.replace("\r\n", "\n").trim

    if !snapshotFile.exists() then
      snapshotFile.getParentFile.mkdirs()
      Files.writeString(snapshotPath, normalizedActual)
      fail(s"Snapshot Baseline created at: ${snapshotPath.toAbsolutePath}. Run again the tests to confirm.")
    else
      val expectedContent = Files.readString(snapshotPath)
      val normalizedExpected = expectedContent.replace("\r\n", "\n").trim

      normalizedActual shouldBe normalizedExpected
package integrations.monad_core.simulator.presentation.support

import scalafx.application.Platform
import javax.imageio.ImageIO
import scalafx.embed.swing.SwingFXUtils
import scalafx.scene.canvas.Canvas
import org.scalatest.matchers.should.Matchers
import scalafx.Includes.jfxImage2sfx
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{CountDownLatch, TimeUnit}

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

  /**
   * does a pixel match with a tolerance to prevent errors given by antialiasing
   */
  private def pixelsMatch(color1: Int, color2: Int, tolerance: Int = 15): Boolean =
    val a1 = (color1 >> 24) & 0xff
    val r1 = (color1 >> 16) & 0xff
    val g1 = (color1 >> 8) & 0xff
    val b1 = color1 & 0xff

    val a2 = (color2 >> 24) & 0xff
    val r2 = (color2 >> 16) & 0xff
    val g2 = (color2 >> 8) & 0xff
    val b2 = color2 & 0xff

    Math.abs(a1 - a2) <= tolerance &&
      Math.abs(r1 - r2) <= tolerance &&
      Math.abs(g1 - g2) <= tolerance &&
      Math.abs(b1 - b2) <= tolerance

  def assertMatchesVisualSnapshot(snapshotName: String, canvas: Canvas, maxAllowedDiffPixels: Int = 50): Unit =
    val snapshot = runOnFxThread {
      canvas.snapshot(null, null)
    }
    val bufferedImage = SwingFXUtils.fromFXImage(snapshot.delegate, null)
    val file = snapshotsDir.resolve(s"$snapshotName.png").toFile

    if !file.exists() then
      file.getParentFile.mkdirs()
      ImageIO.write(bufferedImage, "png", file)
      fail(s"Visual snapshot created at: ${file.getAbsolutePath}")
    else
      val expectedImage = ImageIO.read(file)

      bufferedImage.getWidth shouldBe expectedImage.getWidth
      bufferedImage.getHeight shouldBe expectedImage.getHeight

      val width = bufferedImage.getWidth
      val height = bufferedImage.getHeight

      val diffs = for
        x <- 0 until width
        y <- 0 until height
        act = bufferedImage.getRGB(x, y)
        exp = expectedImage.getRGB(x, y)
        if !pixelsMatch(act, exp)
      yield (x, y, act, exp)

      if diffs.length > maxAllowedDiffPixels then
        val totalPixels = width * height
        val (firstX, firstY, act, exp) = diffs.head
        fail(
          s"Mismatch in '$snapshotName': ${diffs.length}/$totalPixels pixels differ. " +
            s"First discrepancy at ($firstX, $firstY) -> Actual: 0x${act.toHexString.toUpperCase}, Expected: 0x${exp.toHexString.toUpperCase}"
        )

  def runOnFxThread[A](body: => A): A =
    if Platform.isFxApplicationThread then
      body
    else
      var result: Option[Either[Throwable, A]] = None
      val latch = new CountDownLatch(1)

      Platform.runLater {
        try
          result = Some(Right(body))
        catch
          case t: Throwable => result = Some(Left(t))
        finally
          latch.countDown()
      }

      if !latch.await(10, TimeUnit.SECONDS) then
        throw new RuntimeException("Timeout in attesa dell'FX Application Thread")

      result.get match
        case Right(value) => value
        case Left(cause) => throw cause
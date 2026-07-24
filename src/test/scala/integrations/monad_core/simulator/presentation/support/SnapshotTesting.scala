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

  def assertMatchesVisualSnapshot(snapshotName: String, canvas: Canvas): Unit =
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

      val actualPixels = for
        x <- 0 until bufferedImage.getWidth
        y <- 0 until bufferedImage.getHeight
      yield bufferedImage.getRGB(x, y)

      val expectedPixels = for
        x <- 0 until expectedImage.getWidth
        y <- 0 until expectedImage.getHeight
      yield expectedImage.getRGB(x, y)

      actualPixels shouldBe expectedPixels

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
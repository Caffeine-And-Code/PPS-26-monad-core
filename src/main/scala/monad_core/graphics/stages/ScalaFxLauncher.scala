package monad_core.graphics.stages

import monad_core.engine.errors.EngineError
import monad_core.graphics.stages.traits.MainStageBuilder
import monad_core.graphics.{StartupTimeout, UnexpectedStartupFailure}
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.stage.Stage

import java.util.concurrent.{CountDownLatch, TimeUnit}

final class ScalaFxLauncher(mainStage: MainStageBuilder) {

  private val MinStageWidth = 1024.0
  private val MinStageHeight = 720.0
  private val StartupTimeoutSeconds = 10L

  def run(): Either[EngineError, Unit] =
    val latch = new CountDownLatch(1)
    @volatile var result: Either[EngineError, Unit] = Left(StartupTimeout(StartupTimeoutSeconds))

    Platform.startup(() =>
      try
        val stage = new Stage {
          title = "MonadCore2D"
          fullScreen = true
          minWidth = MinStageWidth
          minHeight = MinStageHeight
        }

        val scene = new Scene(900, 600) {
          fill = Color.rgb(25, 26, 28)
        }

        result = mainStage.buildRootContent(scene.width, scene.height) match
          case Right(rootContent) =>
            scene.content = rootContent
            stage.scene = scene
            stage.show()
            Right(())

          case Left(error) =>
            Left(error)
      catch
        case throwable: Throwable =>
          result = Left(UnexpectedStartupFailure(throwable.getMessage))
      finally
        latch.countDown()
    )

    val completedInTime = latch.await(StartupTimeoutSeconds, TimeUnit.SECONDS)

    if !completedInTime then
      Left(StartupTimeout(StartupTimeoutSeconds))
    else
      result
}
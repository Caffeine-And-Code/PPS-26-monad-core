package monad_core.simulator.presentation.stages

import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.World
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.NotificationManager
import monad_core.simulator.presentation.stages.traits.MainStageBuilder
import monad_core.simulator.{StartupTimeout, UnexpectedStartupFailure}
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.stage.Stage

import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

final class ScalaFxLauncher(mainStage: MainStageBuilder) :

  private val MinStageWidth         = 1024.0
  private val MinStageHeight        = 720.0
  private val StartupTimeoutSeconds = 10L

  def run()(using
      aiAgent: AiAgent,
      world: World,
      gameEngineRuntime: GameEngineRuntime
  ): Either[BaseError, Unit] =
    val latch                                     = new CountDownLatch(1)
    @volatile var result: Either[BaseError, Unit] = Left(StartupTimeout(StartupTimeoutSeconds))

    val buildAndShow: Runnable = () =>
      try
        val stage = new Stage {
          title = "MonadCore2D"
          fullScreen = false
          minWidth = MinStageWidth
          minHeight = MinStageHeight
        }

        val notificationLayer = new StackPane {
          pickOnBounds = false
        }

        val scene = new Scene(900, 600) {
          fill = Color.rgb(25, 26, 28)
        }

        result = mainStage
          .buildRootContent(scene.width, scene.height)
          .map { rootContent =>
            scene.content = new StackPane {
              children = Seq(rootContent, notificationLayer)
            }

            NotificationManager.attach(notificationLayer)
            stage.scene = scene
            stage.show()
          }
      catch
        case throwable: Throwable =>
          result = Left(UnexpectedStartupFailure(throwable.getMessage))
      finally latch.countDown()

    try Platform.startup(buildAndShow)
    catch
      case _: IllegalStateException =>
        // JavaFx toolkit has already started, schedule the program startup as soon
        // as possible to the UI thread
        Platform.runLater(buildAndShow)

    val completedInTime = latch.await(StartupTimeoutSeconds, TimeUnit.SECONDS)

    if !completedInTime then Left(StartupTimeout(StartupTimeoutSeconds))
    else result

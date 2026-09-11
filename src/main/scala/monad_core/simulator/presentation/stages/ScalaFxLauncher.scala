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
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Configuration for the JavaFX startup routine.
 *
 * @param timeoutSeconds maximum number of seconds to wait for startup completion
 */
private case class BlockingJfxBootstrapData(timeoutSeconds: Long)

/** Manages initialization and access to the JavaFX application thread. */
private object BlockingJfxBootstrap:

  /**
   * Runs an action on the JavaFX application thread, initializing the toolkit when necessary.
   *
   * @param data startup configuration
   * @param action action to execute on the UI thread
   * @return the action result, `StartupTimeout` if it does not complete in time, or
   *         `UnexpectedStartupFailure` if startup throws an exception
   */
  def run(
      data: BlockingJfxBootstrapData
  )(
      action: () => Either[BaseError, Unit]
  ): Either[BaseError, Unit] =
    val timeoutSeconds = data.timeoutSeconds
    val latch          = new CountDownLatch(1)

    @volatile var result: Either[BaseError, Unit] = Left(StartupTimeout(timeoutSeconds))

    val guardedAction: Runnable = () =>
      try result = action()
      catch
        case throwable: Throwable =>
          result = Left(UnexpectedStartupFailure(throwable.getMessage))
      finally latch.countDown()

    try Platform.startup(guardedAction)
    catch
      case _: IllegalStateException =>
        // JavaFx toolkit has already started, schedule the program startup as soon
        // as possible to the UI thread
        Platform.runLater(guardedAction)

    val completedInTime = latch.await(timeoutSeconds, TimeUnit.SECONDS)

    if !completedInTime then Left(StartupTimeout(timeoutSeconds)) else result

/**
 * Starts the ScalaFX UI and displays the content produced by
 * [[monad_core.simulator.presentation.stages.traits.MainStageBuilder.buildRootContent MainStageBuilder.buildRootContent]].
 *
 * @param mainStage builder for the content displayed in the application window
 */
final class ScalaFxLauncher(mainStage: MainStageBuilder):

  private val MinStageWidth         = 1024.0
  private val MinStageHeight        = 720.0
  private val StartupTimeoutSeconds = 10L

  /** Builds the application stage with its minimum dimensions. */
  private def buildStage(): Stage =
    new Stage {
      title = "MonadCore2D"
      fullScreen = false
      minWidth = MinStageWidth
      minHeight = MinStageHeight
    }

  /** Builds the scene that hosts the main content and notification layer. */
  private def buildScene(): Scene =
    new Scene(900, 600) {
      fill = Color.rgb(25, 26, 28)
    }

  /**
   * Starts the JavaFX application thread, builds the main content, and displays the stage.
   *
   * @param aiAgent contextual AI agent passed to the main-stage builder
   * @param world world associated with the application session
   * @param gameEngineRuntime runtime associated with the application session
   * @return `Left(BaseError)` if startup times out, throws an exception, or the main content cannot be built;
   *         `Right(Unit)` after the stage is shown
   */
  def run()(using
      aiAgent: AiAgent,
      world: World,
      gameEngineRuntime: GameEngineRuntime
  ): Either[BaseError, Unit] =
    BlockingJfxBootstrap.run(
      BlockingJfxBootstrapData(StartupTimeoutSeconds)
    ) { () =>
      val stage = buildStage()
      val scene = buildScene()

      val notificationLayer = new StackPane {
        pickOnBounds = false
      }

      mainStage.buildRootContent(scene.width, scene.height) match
        case Right(rootContent) =>
          scene.content = new StackPane {
            children = Seq(rootContent, notificationLayer)
          }

          NotificationManager.attach(notificationLayer)
          stage.scene = scene
          stage.show()
          Right(())

        case Left(error) =>
          Left(error)
    }

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
 * Contains data that can alter the behavior of the startup routine
 * @param timeoutSeconds the waited time before the startup routine execution expressed in seconds
 */
private case class BlockingJfxBootstrapData(timeoutSeconds: Long)

/**
 * Utility object to encapsulate the management of the ScalaFx UI Thread.
 */
private object BlockingJfxBootstrap:

  /**
   * Run the provided action safely by trying to start the ScalaFx UI Thread,
   * if it's already running the action is scheduled to start as soon as possible.
   *
   *
   * @param data [[BlockingJfxBootstrapData]] containing startup behavior information
   * @param action the executed function on the UI thread
   * @return `Left(BaseError)` if any unexpected error was thrown by the ScalaFx library during startup
   *
   *         `Right(Unit)` if the action was executed correctly
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
 * ScalaFx Entry Point of the application, it wraps the UI Thread management and displays the result
 * of [[MainStageBuilder.buildRootContent()]] provided into a [[Stage]] constructed by the [[buildStage()]] function.
 *
 * @see [[BlockingJfxBootstrap]]
 * @param mainStage the builder who construct the displayed stage inside the main window
 */
final class ScalaFxLauncher(mainStage: MainStageBuilder):

  private val MinStageWidth         = 1024.0
  private val MinStageHeight        = 720.0
  private val StartupTimeoutSeconds = 10L

  /**
   * Build the [[Stage]] with the default values as minWidth and minHeight by [[MinStageWidth]] and [[MinStageHeight]]
   *
   * @return the built [[Stage]]
   */
  private def buildStage(): Stage =
    new Stage {
      title = "MonadCore2D"
      fullScreen = false
      minWidth = MinStageWidth
      minHeight = MinStageHeight
    }

  /**
   * Builds the [[Scene]] used as background and container of the [[MainStageBuilder.buildRootContent()]]
   *
   * @return the built [[Scene]]
   */
  private def buildScene(): Scene =
    new Scene(900, 600) {
      fill = Color.rgb(25, 26, 28)
    }

  /**
   * Starts the ScalaFx UI Thread by invoking [[BlockingJfxBootstrap.run]], to which it provides an action
   * that build the main window of the application.
   *
   * @see [[buildStage()]], [[buildScene()]], [[BlockingJfxBootstrap]]
   * @param aiAgent The [[AiAgent]] that will be provided to the [[MainStageBuilder.buildRootContent()]]
   * @param world The [[World]] that will be provided to the [[MainStageBuilder.buildRootContent()]]
   * @param gameEngineRuntime The [[GameEngineRuntime]] that will be provided to the [[MainStageBuilder.buildRootContent()]]
   * @return `Left(BaseError)` which is carried from [[BlockingJfxBootstrap.run()]] or [[MainStageBuilder.buildRootContent()]]
   *         
   *         `Right(Unit)` when the window is constructed correctly
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

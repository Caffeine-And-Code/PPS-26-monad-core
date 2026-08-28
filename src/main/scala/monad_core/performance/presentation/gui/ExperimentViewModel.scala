package monad_core.performance.presentation.gui

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.domain.PerformanceError
import scalafx.beans.property.ObjectProperty

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Asynchronous boundary used to execute a selected engine performance test. */
type ExperimentExecutor =
  (ExperimentRequest, PhysicsManager) => Future[Either[PerformanceError, String]]

/** Coordinates form submission, physics snapshots, and UI-safe result publication. */
final class ExperimentViewModel(
                                 runner: ExperimentExecutor,
                                 physicsManager: () => PhysicsManager,
                                 runOnUiThread: (() => Unit) => Unit
):

  val state: ObjectProperty[ExperimentState] =
    ObjectProperty(ExperimentState.initial)

  /** Starts one selected performance test when no previous request is running. */
  def onSubmit(values: Map[String, String]): Unit =
    if !state.value.isRunning then
      update(ExperimentActions.onStart)

      ExperimentFormArguments.from(values) match
        case Left(error) =>
          update(ExperimentActions.onComplete(_, Left(error)))
        case Right(request) =>
          val snapshot = physicsManager()
          runner(request, snapshot).onComplete {
            case Success(result) =>
              publish(ExperimentActions.onComplete(_, result))
            case Failure(error) =>
              val message = Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
              publish(ExperimentActions.onFailure(_, message))
          }(ExecutionContext.parasitic)

  /** Publishes one state transition through the graphical UI thread. */
  private def publish(
      action: ExperimentState => ExperimentState
  ): Unit =
    runOnUiThread(() => update(action))

  /** Applies one pure transition to the observable state. */
  private def update(action: ExperimentState => ExperimentState): Unit =
    state.value = action(state.value)

object ExperimentViewModel:

  /** Creates a view model with explicit asynchronous and UI boundaries. */
  def apply(
             runner: ExperimentExecutor,
             physicsManager: () => PhysicsManager,
             runOnUiThread: (() => Unit) => Unit
  ): ExperimentViewModel =
    new ExperimentViewModel(runner, physicsManager, runOnUiThread)

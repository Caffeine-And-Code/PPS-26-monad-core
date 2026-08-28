package monad_core.performance.presentation.gui

import monad_core.performance.domain.PerformanceError

/** Lifecycle of one performance request launched from the graphical interface. */
enum ExperimentState:
  case Ready
  case Running
  case Succeeded(report: String)
  case Failed(message: String)

object ExperimentState:
  /** Initial state before the first request. */
  val initial: ExperimentState = Ready

extension (state: ExperimentState)
  /** Whether a request is currently being executed. */
  def isRunning: Boolean = state == ExperimentState.Running

/** Pure state transitions used by the graphical performance-test view model. */
object ExperimentActions:

  /** Moves a request to running; repeated starts preserve the same state. */
  def onStart(state: ExperimentState): ExperimentState =
    ExperimentState.Running

  /** Applies a domain result only to the request that is currently running. */
  def onComplete(
                  state: ExperimentState,
                  result: Either[PerformanceError, String]
  ): ExperimentState =
    state match
      case ExperimentState.Running =>
        result.fold(
          error => ExperimentState.Failed(error.message),
          ExperimentState.Succeeded.apply
        )
      case _ => state

  /** Applies an unexpected asynchronous failure only to a running request. */
  def onFailure(
                 state: ExperimentState,
                 message: String
  ): ExperimentState =
    state match
      case ExperimentState.Running => ExperimentState.Failed(message)
      case _                                  => state

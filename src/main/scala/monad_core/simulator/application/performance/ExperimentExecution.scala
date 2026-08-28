package monad_core.simulator.application.performance

import monad_core.performance.domain.PerformanceError

import scala.concurrent.Future

/** Immutable command selected and populated by the graphical performance form. */
final case class ExperimentRequest(route: String, arguments: Vector[String])

/** Asynchronous boundary used to execute a selected performance experiment. */
type ExperimentExecutor[Snapshot] =
  (ExperimentRequest, Snapshot) => Future[Either[PerformanceError, String]]

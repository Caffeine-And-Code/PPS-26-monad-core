package monad_core.performance.presentation

import monad_core.performance.application.{ExperimentRunner, NanoClock, PerformanceWorkload}
import monad_core.performance.domain.{ExperimentKind, PerformanceConfig, PerformanceError}

/** Presentation-level entry point for executing and printing performance experiments. */
object PerformanceRuntime:

  /**
   * Runs an experiment and prints its report after successful completion.
   *
   * The printer is not invoked when the runner returns an error.
   *
   * @param kind
   *   performance strategy to execute
   * @param config
   *   validated experiment settings
   * @param workload
   *   workload measured by the experiment
   * @param clock
   *   monotonic clock used for latency sampling
   * @param printer
   *   output boundary receiving the successful report
   * @return
   *   `Right(())` after printing, or the first experiment error
   */
  def handle(kind: ExperimentKind, config: PerformanceConfig)(using
      workload: PerformanceWorkload,
      clock: NanoClock,
      printer: PerformanceReportPrinter
  ): Either[PerformanceError, Unit] =
    ExperimentRunner.run(kind, config).map(printer.print)

package monad_core.performance.presentation

import monad_core.performance.domain.ExperimentReport

/** Console renderer for performance reports. */
object PerformanceConsolePrinter extends PerformanceReportPrinter:

  /**
   * Prints the experiment header, every measurement point, and any detected breakpoint.
   *
   * @param report
   *   completed report to write to console
   */
  override def print(report: ExperimentReport): Unit =
    Console.println(PerformanceReportFormatter.format(report))

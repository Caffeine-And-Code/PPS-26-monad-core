package monad_core.performance.presentation

import monad_core.performance.domain.ExperimentReport

/**
 * Output boundary for completed performance reports.
 *
 * Implementations decide how reports are rendered, keeping experiment orchestration
 * independent of a concrete presentation channel.
 */
trait PerformanceReportPrinter:

  /**
   * Renders or publishes a completed report.
   *
   * @param report
   *   experiment report to present
   */
  def print(report: ExperimentReport): Unit

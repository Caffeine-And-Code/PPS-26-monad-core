package monad_core.performance.presentation

import monad_core.performance.domain.{ExperimentReport, PerformancePoint}

import java.util.Locale

/**
 * Console renderer for performance reports.
 *
 * Latencies are converted from nanoseconds to milliseconds and completion ratios to percentages.
 * Numeric formatting always uses the root locale so output remains stable across environments.
 */
object PerformanceConsolePrinter extends PerformanceReportPrinter:

  /** Nanoseconds per millisecond, used for latency conversion. */
  private val NanosPerMillisecond = 1_000_000.0

  /** Multiplier that converts a ratio to a percentage. */
  private val PercentageFactor = 100.0

  /** Format producing millisecond values with three fractional digits. */
  private val MillisecondsFormat = "%.3f"

  /** Format producing percentage values with two fractional digits. */
  private val PercentageFormat = "%.2f"

  /** Measure unit for milliseconds. */
  private val MillisecondsUnit = " ms"

  /** Measure unit for percentage */
  private val PercentageUnit = "%"

  /**
   * Prints the experiment header, every measurement point, and any detected breakpoint.
   *
   * @param report
   *   completed report to write to standard output
   */
  override def print(report: ExperimentReport): Unit =
    Console.println(s"Performance experiment: ${report.kind}")
    report.points.foreach(printPoint)
    report.breakpoint.foreach(entityCount =>
      Console.println(s"Breakpoint: ${entityCount.value} entities")
    )

  /**
   * Prints all metrics for one entity count.
   *
   * @param point
   *   aggregate performance point to render
   */
  private def printPoint(point: PerformancePoint): Unit =
    Console.println(
      s"""Entities: ${point.entityCount.value}
         |p50: ${formatMilliseconds(point.latency.p50Nanos)}
         |p95: ${formatMilliseconds(point.latency.p95Nanos)}
         |p99: ${formatMilliseconds(point.latency.p99Nanos)}
         |Frame budget completion: ${formatPercentage(
          point.frameBudgetCompletionRate
        )}""".stripMargin
    )

  /**
   * Converts and formats a nanosecond duration as milliseconds.
   *
   * @param nanos
   *   duration in nanoseconds
   * @return
   *   locale-independent decimal value with three fractional digits
   */
  private def formatMilliseconds(nanos: Long): String =
    format(MillisecondsFormat, nanos / NanosPerMillisecond) + MillisecondsUnit

  /**
   * Converts and formats a completion ratio as a percentage.
   *
   * @param rate
   *   completion ratio, conventionally in `[0.0, 1.0]`
   * @return
   *   locale-independent percentage value with two fractional digits, without the percent sign
   */
  private def formatPercentage(rate: Double): String =
    format(PercentageFormat, rate * PercentageFactor) + PercentageUnit

  /**
   * Formats a decimal value with a stable locale.
   *
   * @param pattern
   *   `java.util.Formatter`-compatible floating-point pattern
   * @param value
   *   value to format
   * @return
   *   formatted decimal string using [[java.util.Locale.ROOT]]
   */
  private def format(pattern: String, value: Double): String =
    String.format(Locale.ROOT, pattern, Double.box(value))

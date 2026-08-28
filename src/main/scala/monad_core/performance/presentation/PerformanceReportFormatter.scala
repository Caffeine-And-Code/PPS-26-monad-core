package monad_core.performance.presentation

import monad_core.performance.domain.{DurationConversion, ExperimentReport, PerformancePoint}

import java.util.Locale

/**
 * Pure text formatter shared by console and graphical performance output.
 *
 * Latencies are converted from nanoseconds to milliseconds and completion ratios to percentages.
 * Numeric formatting uses the root locale so reports remain stable across environments.
 */
object PerformanceReportFormatter:

  private val PercentageFactor   = 100.0
  private val MillisecondsFormat = "%.3f"
  private val PercentageFormat   = "%.2f"
  private val MillisecondsUnit   = " ms"
  private val PercentageUnit     = "%"

  /**
   * Formats the experiment header, every measurement point, and any detected breakpoint.
   *
   * @param report
   *   completed report to format
   * @return
   *   complete readable report
   */
  def format(report: ExperimentReport): String =
    val header = Vector(s"Performance experiment: ${report.kind}")
    val points = report.points.map(formatPoint)
    val breakpoint =
      report.breakpoint.map(entityCount => s"Breakpoint: ${entityCount.value} entities")

    (header ++ points ++ breakpoint).mkString("\n")

  /** Formats all metrics for one entity count. */
  private def formatPoint(point: PerformancePoint): String =
    Vector(
      s"Entities: ${point.entityCount.value}",
      s"p50: ${formatMilliseconds(point.latency.p50Nanos)}",
      s"p95: ${formatMilliseconds(point.latency.p95Nanos)}",
      s"p99: ${formatMilliseconds(point.latency.p99Nanos)}",
      s"Frame budget completion: ${formatPercentage(point.frameBudgetCompletionRate)}"
    ).mkString("\n")

  /** Converts and formats a nanosecond duration as milliseconds. */
  private def formatMilliseconds(nanos: Long): String =
    formatDecimal(
      MillisecondsFormat,
      DurationConversion.nanosToMillis(nanos)
    ) + MillisecondsUnit

  /** Converts and formats a completion ratio as a percentage. */
  private def formatPercentage(rate: Double): String =
    formatDecimal(PercentageFormat, rate * PercentageFactor) + PercentageUnit

  /** Formats a decimal value with a stable locale. */
  private def formatDecimal(pattern: String, value: Double): String =
    String.format(Locale.ROOT, pattern, Double.box(value))

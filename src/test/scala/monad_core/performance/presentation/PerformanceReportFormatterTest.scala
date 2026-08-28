package monad_core.performance.presentation

import monad_core.performance.domain.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceReportFormatterTest extends AnyFunSuite with Matchers:

  private val BasePoint = PerformancePoint(
    entityCount = EntityCount.from(100).value,
    latency = LatencyDistribution(1_000_000L, 2_000_000L, 3_000_000L),
    frameBudgetCompletionRate = 0.95
  )

  private val BaseReport = ExperimentReport(
    kind = ExperimentKind.Load,
    points = Vector(BasePoint),
    breakpoint = None
  )

  test("the report formatter formats a complete performance point"):
    val result = PerformanceReportFormatter
      .format(BaseReport)
      .replace("\r\n", "\n")

    val expected = """Performance experiment: Load
                     |Entities: 100
                     |p50: 1.000 ms
                     |p95: 2.000 ms
                     |p99: 3.000 ms
                     |Frame budget completion: 95.00%""".stripMargin
      .replace("\r\n", "\n")

    result shouldBe expected

  test("the report formatter preserves the order of every performance point"):
    val secondPoint = BasePoint.copy(entityCount = EntityCount.from(200).value)
    val report      = BaseReport.copy(points = Vector(BasePoint, secondPoint))

    val result = PerformanceReportFormatter.format(report)

    result.indexOf("Entities: 100") should be < result.indexOf("Entities: 200")

  test("the report formatter appends the stress breakpoint when it is present"):
    val breakpoint = EntityCount.from(200).value
    val report     = BaseReport.copy(breakpoint = Some(breakpoint))

    val result = PerformanceReportFormatter.format(report)

    result should endWith(s"Breakpoint: ${breakpoint.value} entities")

  test("the report formatter omits the stress breakpoint when it is absent"):
    val result = PerformanceReportFormatter.format(BaseReport)

    result should not include "Breakpoint:"

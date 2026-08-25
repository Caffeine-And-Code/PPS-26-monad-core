package monad_core.performance.presentation

import monad_core.performance.domain.*
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

class PerformanceConsolePrinterTest extends AnyFunSuite with Matchers:

  private def consoleOutputToString(report: ExperimentReport): String =
    val output = ByteArrayOutputStream()

    Console.withOut(output):
      PerformanceConsolePrinter.print(report)

    output.toString(StandardCharsets.UTF_8)

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

  test("the console printer should print its header"):
    val printed = consoleOutputToString(BaseReport)

    printed should include(s"Performance experiment: ${BaseReport.kind}")

  test("the console printer includes latency percentiles and frame budget rate"):
    val printed = consoleOutputToString(BaseReport)

    printed should include("p50: 1.000 ms")
    printed should include("p95: 2.000 ms")
    printed should include("p99: 3.000 ms")
    printed should include("Frame budget completion: 95.00%")

  test("the console printer should print every point of a report"):
    val secondPoint = BasePoint.copy(
      entityCount = EntityCount.from(200).value
    )
    val report = BaseReport.copy(points = BaseReport.points :+ secondPoint)

    val printed = consoleOutputToString(report)

    report.points.foreach(point => printed should include(s"Entities: ${point.entityCount.value}"))

  test("the console printer should print every breakpoint of a report"):
    val breakpoint = EntityCount.from(200).value
    val report     = BaseReport.copy(breakpoint = Some(breakpoint))

    val printed = consoleOutputToString(report)

    printed should include(s"Breakpoint: ${breakpoint.value} entities")

  test("the console printer should not print any breakpoints if not presents"):
    val report = BaseReport.copy(breakpoint = None)

    val printed = consoleOutputToString(report)

    printed should not include "Breakpoint:"

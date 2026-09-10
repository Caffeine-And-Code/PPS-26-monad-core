package integrations.monad_core.simulator.infrastructure.performance

import helpers.performance.SequenceNanoClock
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.simulator.application.performance.PerformanceRequest
import monad_core.simulator.domain.performance.{
  EntityCount,
  EntityGrowth,
  EntityRange,
  InvalidPerformanceArgument,
  LatencyDistribution,
  NanoClock,
  PerformanceKind,
  PerformancePlan,
  PerformancePoint,
  PerformanceReport,
  UnknownPerformanceRoute
}
import monad_core.simulator.infrastructure.performance.PerformanceCli
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceCliTest extends AnyFunSuite with Matchers:

  private def parse(route: String, arguments: String*): PerformanceRequest =
    PerformanceCli.parse(route, arguments.toArray).value

  private def option(name: String, value: Any): Seq[String] =
    Seq(name, value.toString)

  private def loadEntities(request: PerformanceRequest): EntityCount =
    request.plan match
      case PerformancePlan.Load(entities) => entities
      case _                              => fail("Expected a load plan")

  private def spikeRange(request: PerformanceRequest): EntityRange =
    request.plan match
      case PerformancePlan.Spike(range) => range
      case _                            => fail("Expected a spike plan")

  private def entityGrowth(request: PerformanceRequest): EntityGrowth =
    request.plan match
      case PerformancePlan.Stress(growth)      => growth
      case PerformancePlan.Scalability(growth) => growth
      case _                                   => fail("Expected an entity-growth plan")

  private def report(
      kind: PerformanceKind = PerformanceKind.Load,
      breakpoint: Option[EntityCount] = None
  ): PerformanceReport =
    val count   = EntityCount.from(10).value
    val latency = LatencyDistribution(1_000_000L, 2_000_000L, 3_000_000L)
    val point   = PerformancePoint(count, latency, 0.5)
    PerformanceReport(kind, Vector(point), breakpoint)

  private val MinimalArguments =
    option(PerformanceCli.Entities, 1) ++
      option(PerformanceCli.MaximumEntities, 1) ++
      option(PerformanceCli.GrowthFactor, 2) ++
      option(PerformanceCli.Iterations, 1) ++
      option(PerformanceCli.Warmups, 0) ++
      option(PerformanceCli.FrameBudgetMillis, 16L)

  private val InvalidArgument = "not-an-argument"

  test("parse maps the load route"):
    val result = parse(PerformanceCli.LoadRoute)

    result.kind shouldBe PerformanceKind.Load

  test("parse maps the stress route"):
    val result = parse(PerformanceCli.StressRoute)

    result.kind shouldBe PerformanceKind.Stress

  test("parse maps the spike route"):
    val result = parse(PerformanceCli.SpikeRoute)

    result.kind shouldBe PerformanceKind.Spike

  test("parse maps the scalability route"):
    val result = parse(PerformanceCli.ScalabilityRoute)

    result.kind shouldBe PerformanceKind.Scalability

  test("parse rejects an unknown route"):
    val result = PerformanceCli.parse("unknown", Array.empty)

    result shouldBe Left(UnknownPerformanceRoute("unknown"))

  test("parse uses the default starting entity count"):
    val result = parse(PerformanceCli.LoadRoute)

    val resultValue = loadEntities(result).value

    resultValue shouldBe PerformanceCli.DefaultStartEntities

  test("parse uses the default maximum entity count"):
    val result = parse(PerformanceCli.SpikeRoute)

    val resultValue = spikeRange(result).maximum.value

    resultValue shouldBe PerformanceCli.DefaultMaximumEntities

  test("parse uses the default growth factor"):
    val result = parse(PerformanceCli.StressRoute)

    val resultValue = entityGrowth(result).factor.value

    resultValue shouldBe PerformanceCli.DefaultGrowthFactor

  test("parse uses the default iteration count"):
    val result = parse(PerformanceCli.LoadRoute)

    val resultValue = result.config.iterations.value

    resultValue shouldBe PerformanceCli.DefaultIterations

  test("parse uses the default warm-up count"):
    val result = parse(PerformanceCli.LoadRoute)

    val resultValue = result.config.warmups.value

    resultValue shouldBe PerformanceCli.DefaultWarmups

  test("parse uses the default frame budget"):
    val result = parse(PerformanceCli.LoadRoute)

    val resultValue = result.config.frameBudget.toMillis

    resultValue shouldBe PerformanceCli.DefaultFrameBudgetMillis

  test("parse reads the starting entity count"):
    val result = parse(PerformanceCli.LoadRoute, option(PerformanceCli.Entities, 7)*)

    val resultValue = loadEntities(result).value

    resultValue shouldBe 7

  test("parse reads the maximum entity count for spike"):
    val arguments = option(PerformanceCli.Entities, 2) ++
      option(PerformanceCli.MaximumEntities, 20)

    val result = parse(PerformanceCli.SpikeRoute, arguments*)

    val resultValue = spikeRange(result).maximum.value

    resultValue shouldBe 20

  test("parse reads the starting entity count for spike"):
    val result = parse(
      PerformanceCli.SpikeRoute,
      option(PerformanceCli.Entities, 3)*
    )

    val resultValue = spikeRange(result).start.value

    resultValue shouldBe 3

  test("parse reads the starting entity count for stress"):
    val result = parse(
      PerformanceCli.StressRoute,
      option(PerformanceCli.Entities, 3)*
    )

    val resultValue = entityGrowth(result).start.value

    resultValue shouldBe 3

  test("parse reads the maximum entity count for stress"):
    val result = parse(
      PerformanceCli.StressRoute,
      option(PerformanceCli.MaximumEntities, 200)*
    )

    val resultValue = entityGrowth(result).maximum.value

    resultValue shouldBe 200

  test("parse reads the growth factor for stress"):
    val result = parse(PerformanceCli.StressRoute, option(PerformanceCli.GrowthFactor, 3)*)

    val resultValue = entityGrowth(result).factor.value

    resultValue shouldBe 3

  test("parse reads the starting entity count for scalability"):
    val result = parse(
      PerformanceCli.ScalabilityRoute,
      option(PerformanceCli.Entities, 3)*
    )

    val resultValue = entityGrowth(result).start.value

    resultValue shouldBe 3

  test("parse reads the maximum entity count for scalability"):
    val result = parse(
      PerformanceCli.ScalabilityRoute,
      option(PerformanceCli.MaximumEntities, 200)*
    )

    val resultValue = entityGrowth(result).maximum.value

    resultValue shouldBe 200

  test("parse reads the growth factor for scalability"):
    val result = parse(
      PerformanceCli.ScalabilityRoute,
      option(PerformanceCli.GrowthFactor, 3)*
    )

    val resultValue = entityGrowth(result).factor.value

    resultValue shouldBe 3

  test("parse reads the iteration count"):
    val result = parse(PerformanceCli.LoadRoute, option(PerformanceCli.Iterations, 3)*)

    val resultValue = result.config.iterations.value

    resultValue shouldBe 3

  test("parse reads the warm-up count"):
    val result = parse(PerformanceCli.LoadRoute, option(PerformanceCli.Warmups, 3)*)

    val resultValue = result.config.warmups.value

    resultValue shouldBe 3

  test("parse reads the frame budget in milliseconds"):
    val result = parse(PerformanceCli.LoadRoute, option(PerformanceCli.FrameBudgetMillis, 20L)*)

    val resultValue = result.config.frameBudget.toMillis

    resultValue shouldBe 20L

  test("parse raises the default maximum to a larger starting count"):
    val start = PerformanceCli.DefaultMaximumEntities + 1

    val result = parse(PerformanceCli.SpikeRoute, option(PerformanceCli.Entities, start)*)

    val resultValue = spikeRange(result).maximum.value

    resultValue shouldBe start

  test("parse raises the default growth maximum to a larger starting count"):
    val start = PerformanceCli.DefaultMaximumEntities + 1

    val result = parse(PerformanceCli.StressRoute, option(PerformanceCli.Entities, start)*)

    val resultValue = entityGrowth(result).maximum.value

    resultValue shouldBe start

  test("parse uses the first occurrence of an argument"):
    val arguments = option(PerformanceCli.Iterations, 2) ++ option(PerformanceCli.Iterations, 3)

    val result = parse(PerformanceCli.LoadRoute, arguments*)

    val resultValue = result.config.iterations.value

    resultValue shouldBe 2

  test("parse uses the default when an argument has no following value"):
    val result = parse(PerformanceCli.LoadRoute, PerformanceCli.Iterations)

    val resultValue = result.config.iterations.value

    resultValue shouldBe PerformanceCli.DefaultIterations

  test("parse ignores an unrelated argument"):
    val result = parse(PerformanceCli.LoadRoute, "--unrelated", "value")

    val resultValue = result.config.iterations.value

    resultValue shouldBe PerformanceCli.DefaultIterations

  test("parse rejects a non-numeric starting entity count"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.Entities, InvalidArgument).toArray
    )

    result shouldBe Left(InvalidPerformanceArgument(PerformanceCli.Entities, InvalidArgument))

  test("parse rejects a non-numeric maximum entity count"):
    val result = PerformanceCli.parse(
      PerformanceCli.SpikeRoute,
      option(PerformanceCli.MaximumEntities, InvalidArgument).toArray
    )

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceCli.MaximumEntities, InvalidArgument)
    )

  test("parse rejects a non-numeric growth factor"):
    val result = PerformanceCli.parse(
      PerformanceCli.StressRoute,
      option(PerformanceCli.GrowthFactor, InvalidArgument).toArray
    )

    result shouldBe Left(InvalidPerformanceArgument(PerformanceCli.GrowthFactor, InvalidArgument))

  test("parse rejects a non-numeric maximum entity count for stress"):
    val result = PerformanceCli.parse(
      PerformanceCli.StressRoute,
      option(PerformanceCli.MaximumEntities, InvalidArgument).toArray
    )

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceCli.MaximumEntities, InvalidArgument)
    )

  test("parse ignores the maximum entity count for load"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.MaximumEntities, InvalidArgument).toArray
    )

    result shouldBe a[Right[?, ?]]

  test("parse ignores the growth factor for load"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.GrowthFactor, InvalidArgument).toArray
    )

    result shouldBe a[Right[?, ?]]

  test("parse ignores the growth factor for spike"):
    val result = PerformanceCli.parse(
      PerformanceCli.SpikeRoute,
      option(PerformanceCli.GrowthFactor, InvalidArgument).toArray
    )

    result shouldBe a[Right[?, ?]]

  test("parse rejects a non-numeric iteration count"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.Iterations, InvalidArgument).toArray
    )

    result shouldBe Left(InvalidPerformanceArgument(PerformanceCli.Iterations, InvalidArgument))

  test("parse rejects a non-numeric warm-up count"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.Warmups, InvalidArgument).toArray
    )

    result shouldBe Left(InvalidPerformanceArgument(PerformanceCli.Warmups, InvalidArgument))

  test("parse rejects a non-numeric frame budget"):
    val result = PerformanceCli.parse(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.FrameBudgetMillis, InvalidArgument).toArray
    )

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceCli.FrameBudgetMillis, InvalidArgument)
    )

  test("format includes the experiment kind"):
    val result = PerformanceCli.format(report(PerformanceKind.Stress))

    result should include("Performance experiment: Stress")

  test("format includes the entity count"):
    val result = PerformanceCli.format(report())

    result should include("Entities: 10")

  test("format includes the median in milliseconds"):
    val result = PerformanceCli.format(report())

    result should include("p50: 1.000 ms")

  test("format includes the ninety-fifth percentile in milliseconds"):
    val result = PerformanceCli.format(report())

    result should include("p95: 2.000 ms")

  test("format includes the ninety-ninth percentile in milliseconds"):
    val result = PerformanceCli.format(report())

    result should include("p99: 3.000 ms")

  test("format includes the frame-budget completion percentage"):
    val result = PerformanceCli.format(report())

    result should include("Frame budget completion: 50.00%")

  test("format includes a present breakpoint"):
    val breakpoint = EntityCount.from(10).value

    val result = PerformanceCli.format(report(breakpoint = Some(breakpoint)))

    result should include("Breakpoint: 10 entities")

  test("format omits an absent breakpoint"):
    val result = PerformanceCli.format(report())

    result should not include "Breakpoint:"

  test("format separates the header from the first point"):
    val result = PerformanceCli.format(report())

    result should include("Performance experiment: Load\nEntities: 10")

  test("format separates the metrics of a point"):
    val result = PerformanceCli.format(report())

    result should include("Entities: 10\np50: 1.000 ms")

  test("runWithClock executes a valid engine command"):
    given NanoClock = SequenceNanoClock(Vector(0L, 1_000L))

    val result = PerformanceCli
      .runWithClock(
        PerformanceCli.LoadRoute,
        MinimalArguments.toArray,
        PhysicsManager.default()
      )
      .value

    result should include("Performance experiment: Load")

  test("run rejects invalid arguments before executing the engine"):
    val result = PerformanceCli.run(
      PerformanceCli.LoadRoute,
      option(PerformanceCli.Entities, InvalidArgument).toArray,
      PhysicsManager.default()
    )

    result shouldBe Left(InvalidPerformanceArgument(PerformanceCli.Entities, InvalidArgument))

  test("run executes with the default physics configuration"):
    val result = PerformanceCli
      .run(
        PerformanceCli.LoadRoute,
        MinimalArguments.toArray
      )
      .value

    result should include("Performance experiment: Load")

  test("runWithRules accepts the current public rule configuration"):
    val rules = EngineFacade
      .physicsRules(PhysicsManager.default())
      .map(_.copy(isEnabled = false))

    val result = PerformanceCli
      .runWithRules(
        PerformanceCli.LoadRoute,
        MinimalArguments.toArray,
        rules
      )
      .value

    result should include("Performance experiment: Load")

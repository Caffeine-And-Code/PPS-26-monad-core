package integrations.monad_core.simulator.infrastructure.performance

import helpers.performance.SequenceNanoClock
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.core.{PhysicsContext, PhysicsError, PhysicsManager, PhysicsRule, PhysicsRuleError, PhysicsRuleResult}
import monad_core.simulator.application.performance.PerformanceRequest
import monad_core.simulator.domain.performance.{EnginePerformanceError, NanoClock, PerformanceConfig, PerformanceKind, PerformancePlan}
import monad_core.simulator.infrastructure.performance.EnginePerformance
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnginePerformanceTest extends AnyFunSuite with Matchers:

  private val Duration = 1_000L

  private def request(): PerformanceRequest =
    val plan   = PerformancePlan.load(1).value
    val config = PerformanceConfig.from(1, 0, 16L).value
    PerformanceRequest(plan, config)

  private def clock(): SequenceNanoClock =
    SequenceNanoClock(Vector(0L, Duration))

  test("run reports the selected experiment kind"):
    given NanoClock = clock()

    val result = EnginePerformance.run(request(), PhysicsManager.default()).value

    result.kind shouldBe PerformanceKind.Load

  test("run measures the deterministic entity count"):
    given NanoClock = clock()

    val result = EnginePerformance.run(request(), PhysicsManager.default()).value

    val resultValue = result.points.head.entityCount.value

    resultValue shouldBe 1

  test("run measures one engine tick"):
    given NanoClock = clock()

    val result = EnginePerformance.run(request(), PhysicsManager.default()).value

    val resultValue = result.points.head.latency.p50Nanos

    resultValue shouldBe Duration

  test("run wraps a physics-rule failure"):
    val failingRule = new PhysicsRule:
      override val RuleId: String = "failing-performance-rule"
      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        Left(PhysicsRuleError("expected"))
    given NanoClock = SequenceNanoClock(Vector(0L))

    val result = EnginePerformance.run(request(), PhysicsManager(Vector(failingRule)))

    result shouldBe Left(EnginePerformanceError("expected"))

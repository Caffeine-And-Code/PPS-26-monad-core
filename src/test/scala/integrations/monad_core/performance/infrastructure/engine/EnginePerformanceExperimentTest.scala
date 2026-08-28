package integrations.monad_core.performance.infrastructure.engine

import helpers.dummies.PhysicsRuleHelper.makeDummyRule
import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.core.{PhysicsManager, PhysicsRuleError}
import monad_core.performance.domain.{
  EnginePerformanceError,
  InvalidPerformanceArgument,
  PerformanceError
}
import monad_core.performance.helpers.SequenceNanoClock
import monad_core.performance.infrastructure.engine.EnginePerformanceExperiment
import monad_core.performance.presentation.{PerformanceArguments, PerformanceRoutes}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EnginePerformanceExperimentTest extends AnyFunSuite with Matchers:

  private val ValidArguments = Vector(
    PerformanceArguments.Entities,
    "1",
    PerformanceArguments.MaximumEntities,
    "1",
    PerformanceArguments.GrowthFactor,
    "2",
    PerformanceArguments.Iterations,
    "1",
    PerformanceArguments.Warmups,
    "0",
    PerformanceArguments.FrameBudgetMillis,
    "16"
  )

  private def run(route: String, clockValues: Vector[Long]): Either[PerformanceError, String] =
    val clock   = SequenceNanoClock(clockValues)
    val physics = PhysicsManager.default().disableAll
    EnginePerformanceExperiment.run(route, ValidArguments, physics)(using clock)

  test("an engine performance test runs the selected load route"):
    val result = run(PerformanceRoutes.Load, Vector(0L, 1L))

    result.map(_.contains("Performance experiment: Load")) shouldBe Right(true)

  test("an engine performance test runs the selected stress route"):
    val result = run(PerformanceRoutes.Stress, Vector(0L, 1L))

    result.map(_.contains("Performance experiment: Stress")) shouldBe Right(true)

  test("an engine performance test runs the selected spike route"):
    val result = run(PerformanceRoutes.Spike, Vector(0L, 1L, 2L, 3L, 4L, 5L))

    result.map(_.contains("Performance experiment: Spike")) shouldBe Right(true)

  test("an engine performance test runs the selected scalability route"):
    val result = run(PerformanceRoutes.Scalability, Vector(0L, 1L))

    result.map(_.contains("Performance experiment: Scalability")) shouldBe Right(true)

  test("an engine performance experiment should fail with the selected physics manager"):
    val failure = PhysicsRuleError("selected physics manager")
    val failingRule = makeDummyRule(
      id = "failing-performance-rule",
      action = (_, _) => Left(failure)
    )
    given CollisionDetector = CollisionDetector.fromGeometry
    val physicsManager      = PhysicsManager(Vector(failingRule))
    val clock               = SequenceNanoClock(Vector(0L))

    val result = EnginePerformanceExperiment.run(
      PerformanceRoutes.Load,
      Vector(PerformanceArguments.Entities, "1"),
      physicsManager
    )(using clock)

    result shouldBe Left(EnginePerformanceError(failure))

  test("an engine performance test returns an invalid command argument"):
    val invalidValue = "invalid"
    val arguments    = Vector(PerformanceArguments.Entities, invalidValue)
    val clock        = SequenceNanoClock(Vector.empty)

    val result = EnginePerformanceExperiment.run(
      PerformanceRoutes.Stress,
      arguments,
      PhysicsManager.default()
    )(using clock)

    result shouldBe Left(
      InvalidPerformanceArgument(PerformanceArguments.Entities, invalidValue)
    )

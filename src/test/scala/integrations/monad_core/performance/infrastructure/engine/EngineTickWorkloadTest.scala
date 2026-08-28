package integrations.monad_core.performance.infrastructure.engine

import helpers.dummies.PhysicsRuleHelper.makeDummyRule
import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.geometry.ShapeCollision.shapeCollidesWithShape
import monad_core.engine.geometry.ShapeContainment.shapeContainsPoint
import monad_core.engine.physics.core.{PhysicsManager, PhysicsRuleError}
import monad_core.performance.application.SampleCollector
import monad_core.performance.domain.{EnginePerformanceError, EntityCount, PerformanceError}
import monad_core.performance.infrastructure.engine.EngineTickWorkload
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EngineTickWorkloadTest extends AnyFunSuite with Matchers:

  private def prepareWorkload(
      entityCount: Int
  ): Either[PerformanceError, SampleCollector.Operation] =
    EntityCount.from(entityCount).flatMap(EngineTickWorkload.prepare)

  test("prepare should initialize the workload correctly"):
    val entityCount = 10

    val result =
      prepareWorkload(entityCount)
        .flatMap(operation => operation())

    result shouldBe Right(())

  test("a workload should fail when its Physics Manager fails"):
    val failure = PhysicsRuleError("selected physics manager")
    val failingRule = makeDummyRule(
      id = "failing-performance-rule",
      action = (_, _) => Left(failure)
    )

    given CollisionDetector = CollisionDetector.fromGeometry
    val physicsManager      = PhysicsManager(Vector(failingRule))
    val workload            = EngineTickWorkload.withPhysicsManager(physicsManager)
    val entityCount         = EntityCount.from(1)

    val result = entityCount.flatMap(workload.prepare).flatMap(operation => operation())

    result shouldBe Left(EnginePerformanceError(failure))

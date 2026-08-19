package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsRuleTest extends AnyFunSuite with Matchers:
  test("two PhysicsRule instances with the same id should be equal"):
    val rule1: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"
      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(scene))

    val rule2: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"
      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(scene))

    rule1 == rule2 shouldBe true

  test("two PhysicsRule instances with different ids should not be equal"):
    val rule1: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"

      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(scene))

    val rule2: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule2"

      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(scene))

    rule1 == rule2 shouldBe false

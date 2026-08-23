package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.helper.PhysicsRuleHelper
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

  test("a PhysicsRule should have an empty id by default"):
    val rule: PhysicsRule = new PhysicsRule:
      override def apply(scene: State, dt: Long)(using
          detector: CollisionDetector
      ): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(scene))

    rule.RuleId shouldBe ""

  test("two PhysicsRule instances with empty ids should not be equal"):
    val rule1 = PhysicsRuleHelper.makeDummyRule("")
    val rule2 = PhysicsRuleHelper.makeDummyRule("")

    rule1 == rule2 shouldBe false

  test("a PhysicsRule with an empty id should not equal one with a non-empty id"):
    val emptyIdRule = PhysicsRuleHelper.makeDummyRule("")

    val namedRule: PhysicsRule = PhysicsRuleHelper.makeDummyRule("rule1")

    emptyIdRule == namedRule shouldBe false
    namedRule == emptyIdRule shouldBe false

  test("a PhysicsRule should equal itself"):
    val rule: PhysicsRule = PhysicsRuleHelper.makeDummyRule("rule1")

    rule == rule shouldBe true

  test("a PhysicsRule should not equal null"):
    val rule: PhysicsRule = PhysicsRuleHelper.makeDummyRule("rule1")

    rule == null shouldBe false

  test("a PhysicsRule with a non-empty id should use the id hash code"):
    val rule: PhysicsRule = PhysicsRuleHelper.makeDummyRule("rule1")

    rule.hashCode() shouldBe "rule1".hashCode

  test("a PhysicsRule with an empty id should use an hash code based on an empty string"):
    val rule = PhysicsRuleHelper.makeDummyRule("")

    rule.hashCode() shouldBe "".hashCode

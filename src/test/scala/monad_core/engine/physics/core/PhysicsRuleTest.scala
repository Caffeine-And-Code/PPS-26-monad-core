package monad_core.engine.physics.core

import helpers.dummies.PhysicsRuleHelper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsRuleTest extends AnyFunSuite with Matchers:
  test("two PhysicsRule instances with the same id should be equal"):
    val rule1: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"
      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(context.state))

    val rule2: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"
      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(context.state))

    rule1 == rule2 shouldBe true

  test("two PhysicsRule instances with different ids should not be equal"):
    val rule1: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule1"

      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(context.state))

    val rule2: PhysicsRule = new PhysicsRule:
      override val RuleId: String = "rule2"

      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        Right(PhysicsRuleResult(context.state))

    rule1 == rule2 shouldBe false

  test("a PhysicsRule should have an empty id by default"):
    val rule: PhysicsRule = (context: PhysicsContext) => Right(PhysicsRuleResult(context.state))

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

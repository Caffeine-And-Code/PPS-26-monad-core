package engine.physics.core

import engine.physics.combinators.RuleCombinator
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RuleCombinatorTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  given TestDetector = mock[TestDetector]
  private val DeltaTimeOneSecond = 1_000_000_000L

  private def makeDummyRule(f: TestScene => Either[PhysicsError, TestScene]): PhysicsRule[TestScene, TestDetector] =
    new PhysicsRule[TestScene, TestDetector]:
      override def apply(scene: TestScene)(using detector: TestDetector, dt: Long): Either[PhysicsError, TestScene] =
        f(scene)

  test("sequence should return unchanged scene when rules list is empty"):
    val initialScene = mock[TestScene]
    val compositeRule = RuleCombinator.sequence[TestScene, TestDetector](Seq.empty)

    val result = compositeRule(initialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe initialScene

  test("sequence should apply rules in order and pass updated scene to next rule"):
    val scene0 = mock[TestScene]
    val scene1 = mock[TestScene]
    val scene2 = mock[TestScene]

    val rule1 = makeDummyRule: scene =>
      scene shouldBe scene0
      Right(scene1)

    val rule2 = makeDummyRule: scene =>
      scene shouldBe scene1
      Right(scene2)

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2))
    val result = compositeRule(scene0)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe scene2

  test("sequence should short-circuit and stop execution on first error"):
    val scene0 = mock[TestScene]
    val scene1 = mock[TestScene]
    val expectedError = PhysicsRuleError("Rule 2 failed")

    val rule1 = makeDummyRule(_ => Right(scene1))
    val rule2 = makeDummyRule(_ => Left(expectedError))

    val rule3 = mock[PhysicsRule[TestScene, TestDetector]]

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2, rule3))
    val result = compositeRule(scene0)(using summon[TestDetector], DeltaTimeOneSecond)

    result shouldBe Left(expectedError)

  test("andThen / + extensions should correctly compose two rules"):
    import RuleCombinator.*

    val scene0 = mock[TestScene]
    val scene1 = mock[TestScene]
    val scene2 = mock[TestScene]

    val rule1 = makeDummyRule(_ => Right(scene1))
    val rule2 = makeDummyRule(_ => Right(scene2))

    val composedRule = rule1 + rule2
    val result = composedRule(scene0)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe scene2
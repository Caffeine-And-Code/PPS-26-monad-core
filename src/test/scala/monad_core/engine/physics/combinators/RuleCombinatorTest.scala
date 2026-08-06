package monad_core.engine.physics.combinators

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.traits.State
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.combinators.RuleCombinator.*
import monad_core.engine.physics.core.{PhysicsError, PhysicsRule, PhysicsRuleError}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RuleCombinatorTest extends AnyFunSuite with Matchers with MockFactory:

  given CollisionDetector = mock[CollisionDetector]
  private val DeltaTimeOneSecond = 1_000_000_000L

  private def makeDummyRule(f: State => Either[PhysicsError, State]): PhysicsRule =
    new PhysicsRule:
      override def apply(scene: State, dt: Long)(using detector: CollisionDetector): Either[PhysicsError, State] =
        f(scene)

  private val Scene0 = mock[State]
  private val Scene1 = mock[State]
  private val Scene2 = mock[State]

  test("sequence should return unchanged scene when rules list is empty"):
    val compositeRule = RuleCombinator.sequence(Seq.empty)

    val result = compositeRule(Scene0, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    result shouldBe Scene0

  test("sequence should apply rules in order and pass updated scene to next rule"):

    val rule1 = makeDummyRule: scene =>
      scene shouldBe Scene0
      Right(Scene1)

    val rule2 = makeDummyRule: scene =>
      scene shouldBe Scene1
      Right(Scene2)

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2))
    val result = compositeRule(Scene0, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    result shouldBe Scene2

  test("sequence should short-circuit and stop execution on first error"):
    val expectedError = PhysicsRuleError("Rule 2 failed")

    val rule1 = makeDummyRule(_ => Right(Scene1))
    val rule2 = makeDummyRule(_ => Left(expectedError))

    val rule3 = mock[PhysicsRule]

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2, rule3))
    val result = compositeRule(Scene0, DeltaTimeOneSecond)(using summon[CollisionDetector])

    result shouldBe Left(expectedError)
  
  test("+ operator for rules should correctly compose two rules"):
    
    val rule1 = makeDummyRule(_ => Right(Scene1))
    val rule2 = makeDummyRule(_ => Right(Scene2))

    val composedRule = rule1 + rule2
    val result = composedRule(Scene0, DeltaTimeOneSecond)(using summon[CollisionDetector]).value

    result shouldBe Scene2
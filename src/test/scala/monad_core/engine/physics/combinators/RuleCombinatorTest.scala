package monad_core.engine.physics.combinators

import monad_core.engine.core.events.EngineEvent.EntityCreated
import monad_core.engine.core.traits.State
import monad_core.engine.helper.PhysicsConstantHelper.DeltaTimeOneSecond
import monad_core.engine.helper.PhysicsRuleHelper.makeDummyRule
import monad_core.engine.model.{Entity, Vector2D}
import monad_core.engine.physics.combinators.RuleCombinator
import monad_core.engine.physics.combinators.RuleCombinator.*
import monad_core.engine.physics.core.{
  CollisionSnapshot,
  PhysicsError,
  PhysicsContext,
  PhysicsRule,
  PhysicsRuleError,
  PhysicsRuleResult,
  SurfaceContact
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RuleCombinatorTest extends AnyFunSuite with Matchers with MockFactory:

  private val Scene0 = mock[State]
  private val Scene1 = mock[State]
  private val Scene2 = mock[State]

  test("sequence should return unchanged scene when rules list is empty"):
    val compositeRule = RuleCombinator.sequence(Seq.empty)

    val result = compositeRule(PhysicsContext(Scene0, DeltaTimeOneSecond)).value

    result shouldBe PhysicsRuleResult(Scene0)

  test("sequence should apply rules in order and pass updated scene to next rule"):

    val rule1 = makeDummyRule(action = (scene, deltaTime) => Right(Scene1))

    val rule2 = makeDummyRule(action = (scene, deltaTime) => Right(Scene2))

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2))
    val result        = compositeRule(PhysicsContext(Scene0, DeltaTimeOneSecond)).value

    result shouldBe PhysicsRuleResult(Scene2)

  test("sequence should accumulate rule events in execution order"):
    val entity1 = Entity.circle("entity-1", Vector2D(0, 0), 1).value
    val entity2 = Entity.circle("entity-2", Vector2D(1, 0), 1).value
    val event1  = EntityCreated(entity1)
    val event2  = EntityCreated(entity2)
    val rule1   = makeDummyRule(action = (_, _) => Right(Scene1), events = Vector(event1))
    val rule2   = makeDummyRule(action = (_, _) => Right(Scene2), events = Vector(event2))

    val result = RuleCombinator
      .sequence(Seq(rule1, rule2))(PhysicsContext(Scene0, DeltaTimeOneSecond))
      .value

    result shouldBe PhysicsRuleResult(Scene2, Vector(event1, event2))

  test("sequence should preserve the collision snapshot while the state evolves"):
    val entity   = Entity.circle("entity", Vector2D(0, 0), 1).value
    val snapshot = CollisionSnapshot(surfaceContacts = Vector(SurfaceContact(entity.id, entity.id)))
    val context  = PhysicsContext(Scene0, DeltaTimeOneSecond, snapshot)
    val inspect  = mockFunction[PhysicsContext, Either[PhysicsError, PhysicsRuleResult]]
    val first    = makeDummyRule(action = (_, _) => Right(Scene1))
    val second = new PhysicsRule:
      override def apply(context: PhysicsContext): Either[PhysicsError, PhysicsRuleResult] =
        inspect(context)

    inspect
      .expects(context.copy(state = Scene1))
      .returning(Right(PhysicsRuleResult(Scene2)))
      .once()

    RuleCombinator.sequence(Seq(first, second))(context).value.state shouldBe Scene2

  test("sequence should short-circuit and stop execution on first error"):
    val expectedError = PhysicsRuleError("Rule 2 failed")

    val rule1 = makeDummyRule(action = (scene, deltaTime) => Right(Scene1))
    val rule2 = makeDummyRule(action = (scene, deltaTime) => Left(expectedError))

    val rule3 = mock[PhysicsRule]

    val compositeRule = RuleCombinator.sequence(Seq(rule1, rule2, rule3))
    val result        = compositeRule(PhysicsContext(Scene0, DeltaTimeOneSecond))

    result shouldBe Left(expectedError)

  test("+ operator for rules should correctly compose two rules"):

    val rule1 = makeDummyRule(action = (scene, deltaTime) => Right(Scene1))
    val rule2 = makeDummyRule(action = (scene, deltaTime) => Right(Scene2))

    val composedRule = rule1 + rule2
    val result       = composedRule(PhysicsContext(Scene0, DeltaTimeOneSecond)).value

    result shouldBe PhysicsRuleResult(Scene2)

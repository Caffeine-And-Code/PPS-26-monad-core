package monad_core.engine.physics.core

import monad_core.engine.collision_detection.CollisionDetector
import monad_core.engine.core.events.EngineEvent.{CollisionDetected, EntityUpdated}
import monad_core.engine.core.events.CollisionTarget
import monad_core.engine.core.traits.State
import monad_core.engine.geometry.Collision
import monad_core.engine.helper.MockStateHelper
import monad_core.engine.model.{Entity, Scene, Vector2D}
import monad_core.engine.helper.PhysicsConstantHelper.DeltaTimeOneSecond
import monad_core.engine.helper.PhysicsRuleHelper.makeDummyRule
import monad_core.engine.physics.rules.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsManagerTest extends AnyFunSuite with Matchers with MockFactory with MockStateHelper:

  private val MockAction       = mockFunction[State, Long, Either[PhysicsError, State]]
  private val DefaultRuleCount = 5

  private val Rule1Id = "rule1"
  private val Rule2Id = "rule2"
  private val Rule3Id = "rule3"

  private val Rule1 = makeDummyRule(Rule1Id, MockAction)
  private val Rule2 = makeDummyRule(Rule2Id, MockAction)
  private val Rule3 = makeDummyRule(Rule3Id, MockAction)

  private val DefaultManager = PhysicsManager.default()

  given MockDetector: CollisionDetector = mock[CollisionDetector]

  test("apply should create a PhysicsEngine with all provided rules enabled by default"):
    val manager = PhysicsManager(Vector(Rule1, Rule2))

    manager.isEnabled(Rule1) shouldBe true
    manager.isEnabled(Rule2) shouldBe true
    manager.rules shouldBe Vector(Rule1, Rule2)

  test("default should create an engine with the standard rules initialized and enabled"):
    DefaultManager.rules should have size DefaultRuleCount
    DefaultManager.enabledRules should have size DefaultRuleCount

    DefaultManager.isEnabled(EnemyAttractionRule.enemyAttractionRule) shouldBe true
    DefaultManager.isEnabled(SurfaceDynamicsRule.surfaceDynamicsRule) shouldBe true
    DefaultManager.isEnabled(CollisionResolutionRule.collisionResolutionRule) shouldBe true
    DefaultManager.isEnabled(BorderContactRule.borderContactRule) shouldBe true
    DefaultManager.isEnabled(KinematicsRule.kinematicsRule) shouldBe true

  test("isEnabled should return true if the rule is active, false otherwise"):
    val MissingRuleId = "missingRule"
    val missingRule   = makeDummyRule(MissingRuleId, MockAction)
    val manager       = PhysicsManager(Vector(Rule1))

    manager.isEnabled(Rule1) shouldBe true
    manager.isEnabled(missingRule) shouldBe false

  test("disable should remove a specific rule from the enabled set without affecting others"):
    val initialManager = PhysicsManager(Vector(Rule1, Rule2))

    val updatedManager = initialManager.disable(Rule1)

    updatedManager.isEnabled(Rule1) shouldBe false
    updatedManager.isEnabled(Rule2) shouldBe true

  test("disable should correctly deactivate a specific default rule"):
    val updatedManager = DefaultManager.disable(KinematicsRule.kinematicsRule)

    updatedManager.isEnabled(KinematicsRule.kinematicsRule) shouldBe false
    DefaultManager.isEnabled(EnemyAttractionRule.enemyAttractionRule) shouldBe true
    DefaultManager.isEnabled(SurfaceDynamicsRule.surfaceDynamicsRule) shouldBe true
    DefaultManager.isEnabled(CollisionResolutionRule.collisionResolutionRule) shouldBe true
    DefaultManager.isEnabled(BorderContactRule.borderContactRule) shouldBe true

  test("enable should add a specific rule to the enabled set"):
    val initialManager = PhysicsManager(Vector(Rule1)).disable(Rule1)

    val updatedManager = initialManager.enable(Rule1)

    initialManager.isEnabled(Rule1) shouldBe false
    updatedManager.isEnabled(Rule1) shouldBe true

  test("disableAll should clear the enabled set, deactivating all rules"):
    val initialManager = PhysicsManager(Vector(Rule1, Rule2))

    val updatedManager = initialManager.disableAll

    updatedManager.isEnabled(Rule1) shouldBe false
    updatedManager.isEnabled(Rule2) shouldBe false
    updatedManager.enabledRules shouldBe empty

  test("enableAll should activate all rules originally provided to the engine"):
    val initialManager = PhysicsManager(Vector(Rule1, Rule2)).disableAll

    val updatedManager = initialManager.enableAll

    updatedManager.isEnabled(Rule1) shouldBe true
    updatedManager.isEnabled(Rule2) shouldBe true
    updatedManager.enabledRules should contain theSameElementsAs Set(Rule1, Rule2)

  test("step should execute all enabled rules sequentially passing the updated scene at each step"):
    val action1 = MockAction
    val action2 = MockAction

    val rule1   = makeDummyRule(Rule1Id, MockAction)
    val rule2   = makeDummyRule(Rule2Id, MockAction)
    val manager = PhysicsManager(Vector(rule1, rule2))

    val initialScene    = stateWithEntities(List.empty)
    val sceneAfterRule1 = stateWithEntities(List.empty)
    val finalScene      = stateWithEntities(List.empty)

    action1.expects(initialScene, DeltaTimeOneSecond).returning(Right(sceneAfterRule1)).once()
    action2.expects(sceneAfterRule1, DeltaTimeOneSecond).returning(Right(finalScene)).once()

    val result = manager.step(initialScene, DeltaTimeOneSecond).value

    result.state shouldBe finalScene
    result.events shouldBe empty

  test("step should skip disabled rules and continue the execution sequence"):
    val action1 = MockAction
    val action2 = MockAction
    val action3 = MockAction

    val rule1 = makeDummyRule(Rule1Id, MockAction)
    val rule2 = makeDummyRule(Rule2Id, MockAction)
    val rule3 = makeDummyRule(Rule3Id, MockAction)

    val manager = PhysicsManager(Vector(rule1, rule2, rule3)).disable(rule2)

    val initialScene    = stateWithEntities(List.empty)
    val sceneAfterRule1 = stateWithEntities(List.empty)
    val finalScene      = stateWithEntities(List.empty)

    action1.expects(initialScene, DeltaTimeOneSecond).returning(Right(sceneAfterRule1)).once()
    action2.expects(*, *).never()
    action3.expects(sceneAfterRule1, DeltaTimeOneSecond).returning(Right(finalScene)).once()

    val result = manager.step(initialScene, DeltaTimeOneSecond).value

    result.state shouldBe finalScene
    result.events shouldBe empty

  test("step should short-circuit execution and return the error if a rule fails"):
    val action1 = MockAction
    val action2 = MockAction

    val rule1   = makeDummyRule(Rule1Id, MockAction)
    val rule2   = makeDummyRule(Rule2Id, MockAction)
    val manager = PhysicsManager(Vector(rule1, rule2))

    val initialScene = stateWithEntities(List.empty)

    action1
      .expects(initialScene, DeltaTimeOneSecond)
      .returning(Left(PhysicsRuleError("Test error")))
      .once()
    action2.expects(*, *).never()

    val result = manager.step(initialScene, DeltaTimeOneSecond)

    result shouldBe Left(PhysicsRuleError("Test error"))

  test("step should emit an EntityUpdated event with the previous and current entity"):
    val initialEntity = Entity.circle("entity", Vector2D(10, 10), 1).value
    val updatedEntity = initialEntity.moveTo(Vector2D(20, 20))
    val initialScene  = Scene(entities = Map(initialEntity.id -> initialEntity))
    val updatedScene  = Scene(entities = Map(updatedEntity.id -> updatedEntity))
    val rule          = makeDummyRule(Rule1Id, MockAction)
    val manager       = PhysicsManager(Vector(rule))

    MockAction
      .expects(initialScene, DeltaTimeOneSecond)
      .returning(Right(updatedScene))
      .once()

    val result = manager.step(initialScene, DeltaTimeOneSecond).value

    result.state shouldBe updatedScene
    result.events shouldBe Vector(EntityUpdated(initialEntity, updatedEntity))

  test("step should emit a collision event before entity state events"):
    val mobileEntity = Entity
      .circle("a-mobile", Vector2D(10, 10), 2)
      .value
      .withSpeed(Vector2D(1, 0))
    val fixedEntity = Entity.circle("b-fixed", Vector2D(12, 10), 2).value
    val scene = Scene(
      entities = Map(
        mobileEntity.id -> mobileEntity,
        fixedEntity.id  -> fixedEntity
      )
    )
    val collision = Collision(Vector2D(1, 0), 2)
    val collisionEvent =
      CollisionDetected(mobileEntity.id, CollisionTarget.Entity(fixedEntity.id), collision)
    val collisionRule = makeDummyRule(
      CollisionResolutionRule.collisionResolutionRule.RuleId,
      MockAction,
      events = Vector(collisionEvent)
    )
    val manager = PhysicsManager(Vector(collisionRule))

    MockAction.expects(scene, DeltaTimeOneSecond).returning(Right(scene)).once()

    val result = manager.step(scene, DeltaTimeOneSecond).value

    result.events shouldBe Vector(collisionEvent)

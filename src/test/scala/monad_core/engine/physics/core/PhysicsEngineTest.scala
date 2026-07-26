package monad_core.engine.physics.core

import monad_core.engine.physics.rules.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsEngineTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  def generateMockRule (id: String, action: (TestScene, Long) => Either[PhysicsError, TestScene]): PhysicsRule[TestScene, TestDetector] =
    new PhysicsRule[TestScene, TestDetector]:
      override val ruleId: String = id
      override def apply(scene: TestScene)(using detector: TestDetector, dt: Long): Either[PhysicsError, TestScene] =
        action(scene, dt)

  private val MockAction = mockFunction[TestScene, Long, Either[PhysicsError, TestScene]]
  private val DefaultRuleCount = 4
  private val DeltaTime = 16L

  private val Rule1Id = "rule1"
  private val Rule2Id = "rule2"
  private val Rule3Id = "rule3"
  private val MissingRuleId = "missingRule"

  given TestDetector = mock[TestDetector]
  given PhysicsState[TestScene] = mock[PhysicsState[TestScene]]
  given SurfaceDetection[TestDetector] = mock[SurfaceDetection[TestDetector]]
  given CollisionResolutionDetection[TestDetector] = mock[CollisionResolutionDetection[TestDetector]]

  test("apply should create a PhysicsEngine with all provided rules enabled by default"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)

    val engine = PhysicsEngine(Vector(rule1, rule2))

    engine.isEnabled(rule1) shouldBe true
    engine.isEnabled(rule2) shouldBe true
    engine.rules shouldBe Vector(rule1, rule2)

  test("default should create an engine with the standard 4 rules initialized and enabled"):
    val engine = PhysicsEngine.default[TestScene, TestDetector]

    engine.rules should have size DefaultRuleCount
    engine.enabledRules should have size DefaultRuleCount

    engine.isEnabled(EnemyAttractionRule.enemyAttractionRule) shouldBe true
    engine.isEnabled(SurfaceDynamicsRule.surfaceDynamicsRule) shouldBe true
    engine.isEnabled(CollisionResolutionRule.collisionResolutionRule) shouldBe true
    engine.isEnabled(KinematicsRule.kinematicsRule) shouldBe true

  test("isEnabled should return true if the rule is active, false otherwise"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val missingRule = generateMockRule(MissingRuleId, MockAction)
    val engine = PhysicsEngine(Vector(rule1))

    engine.isEnabled(rule1) shouldBe true
    engine.isEnabled(missingRule) shouldBe false

  test("disable should remove a specific rule from the enabled set without affecting others"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val initialEngine = PhysicsEngine(Vector(rule1, rule2))

    val updatedEngine = initialEngine.disable(rule1)

    updatedEngine.isEnabled(rule1) shouldBe false
    updatedEngine.isEnabled(rule2) shouldBe true

  test("disable should correctly deactivate a specific default rule"):
    val engine = PhysicsEngine.default[TestScene, TestDetector]

    val updatedEngine = engine.disable(KinematicsRule.kinematicsRule)

    updatedEngine.isEnabled(KinematicsRule.kinematicsRule) shouldBe false
    updatedEngine.isEnabled(EnemyAttractionRule.enemyAttractionRule) shouldBe true

  test("enable should add a specific rule to the enabled set"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val initialEngine = PhysicsEngine(Vector(rule1)).disable(rule1)

    initialEngine.isEnabled(rule1) shouldBe false

    val updatedEngine = initialEngine.enable(rule1)

    updatedEngine.isEnabled(rule1) shouldBe true

  test("disableAll should clear the enabled set, deactivating all rules"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val initialEngine = PhysicsEngine(Vector(rule1, rule2))

    val updatedEngine = initialEngine.disableAll

    updatedEngine.isEnabled(rule1) shouldBe false
    updatedEngine.isEnabled(rule2) shouldBe false
    updatedEngine.enabledRules shouldBe empty

  test("enableAll should activate all rules originally provided to the engine"):
    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val initialEngine = PhysicsEngine(Vector(rule1, rule2)).disableAll

    val updatedEngine = initialEngine.enableAll

    updatedEngine.isEnabled(rule1) shouldBe true
    updatedEngine.isEnabled(rule2) shouldBe true
    updatedEngine.enabledRules should contain theSameElementsAs Set(rule1, rule2)

  test("step should execute all enabled rules sequentially passing the updated scene at each step"):
    val action1 = MockAction
    val action2 = MockAction

    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val engine = PhysicsEngine(Vector(rule1, rule2))

    val initialScene = mock[TestScene]
    val sceneAfterRule1 = mock[TestScene]
    val finalScene = mock[TestScene]

    action1.expects(initialScene, DeltaTime).returning(Right(sceneAfterRule1)).once()
    action2.expects(sceneAfterRule1, DeltaTime).returning(Right(finalScene)).once()

    val result = engine.step(initialScene, DeltaTime).value

    result shouldBe finalScene

  test("step should skip disabled rules and continue the execution sequence"):
    val action1 = MockAction
    val action2 = MockAction
    val action3 = MockAction

    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val rule3 = generateMockRule(Rule3Id, MockAction)

    val engine = PhysicsEngine(Vector(rule1, rule2, rule3)).disable(rule2)

    val initialScene = mock[TestScene]
    val sceneAfterRule1 = mock[TestScene]
    val finalScene = mock[TestScene]

    action1.expects(initialScene, DeltaTime).returning(Right(sceneAfterRule1)).once()
    action2.expects(*, *).never()
    action3.expects(sceneAfterRule1, DeltaTime).returning(Right(finalScene)).once()

    val result = engine.step(initialScene, DeltaTime).value

    result shouldBe finalScene

  test("step should short-circuit execution and return the error if a rule fails"):
    val action1 = MockAction
    val action2 = MockAction

    val rule1 = generateMockRule(Rule1Id, MockAction)
    val rule2 = generateMockRule(Rule2Id, MockAction)
    val engine = PhysicsEngine(Vector(rule1, rule2))

    val initialScene = mock[TestScene]

    action1.expects(initialScene, DeltaTime).returning(Left(PhysicsRuleError("Test error"))).once()
    action2.expects(*, *).never()

    val result = engine.step(initialScene, DeltaTime)

    result shouldBe Left(PhysicsRuleError("Test error"))
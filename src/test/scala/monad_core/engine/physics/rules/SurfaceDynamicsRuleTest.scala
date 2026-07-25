package monad_core.engine.physics.rules

import engine.model.*
import monad_core.engine.model.Entity.*
import engine.physics.core.*
import monad_core.engine.model.{Entity, LocatableId, Surface, Vector2D}
import monad_core.engine.physics.core.{NegativeDeltaTime, PhysicsRule, PhysicsState}
import monad_core.engine.physics.rules.SurfaceDetection
import monad_core.engine.physics.rules.SurfaceDynamicsRule.given
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SurfaceDynamicsRuleTest extends AnyFunSuite with Matchers with MockFactory:

  trait TestScene
  trait TestDetector

  given TestDetector = mock[TestDetector]
  given SurfaceDetection[TestDetector] = mock[SurfaceDetection[TestDetector]]

  private val DeltaTimeOneSecond = 1_000_000_000L
  private val NegativeDt = -1L
  private val EntityRadius = 1.0
  private val InitialScene = mock[TestScene]

  private def makeEntity(id: String, position: Vector2D): Entity =
    Entity.circle(id = id, position = position, radius = EntityRadius).value

  private def makeMovingEntity(id: String, position: Vector2D, speed: Vector2D): Entity =
    makeEntity(id, position).withSpeed(speed).value

  private def makeSurface(id: String, position: Vector2D, radius: Double): Surface =
    Surface.circle(id = id, position = position, radius = radius).value

  private def addWeight(entity: Entity, weight: Int): Entity =
    entity.withWeight(weight).value

  test("the rule should return NegativeDeltaTime when delta time is negative"):
    val mockState = mock[PhysicsState[TestScene]]
    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], NegativeDt)

    result.shouldBe(Left(NegativeDeltaTime(NegativeDt)))

  test("the rule should return the unchanged scene when there are no entities"):
    val mockState = mock[PhysicsState[TestScene]]

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map.empty)
      .once()

    (mockState.getSurfaces(_: TestScene))
      .expects(InitialScene)
      .returning(Map.empty)
      .once()

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when it has no speed"):
    val entityId = "entity"
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val surfaceId = "surface"
    val surfacePositionX = 0.0
    val surfacePositionY = 0.0
    val surfacePosition = Vector2D(surfacePositionX, surfacePositionY)
    val surfaceRadius = 5.0
    val surfaceForceX = 10.0
    val surfaceForceY = 0.0
    val surfaceForce = Vector2D(surfaceForceX, surfaceForceY)
    val surfaceFrictionIndex = 0.1

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[SurfaceDetection[TestDetector]]

    val entity = makeEntity(id = entityId, position = entityPosition)
    val surface = makeSurface(id = surfaceId, position = surfacePosition, radius = surfaceRadius)
      .withAppliedForce(surfaceForce).value
      .withFrictionIndex(surfaceFrictionIndex).value

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity.id -> entity))
      .once()

    (mockState.getSurfaces(_: TestScene))
      .expects(InitialScene)
      .returning(Map(surface.id -> surface))
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when it is outside the surface"):
    val entityId = "entity"
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entitySpeedX = 1.0
    val entitySpeedY = 1.0
    val entitySpeed = Vector2D(entitySpeedX, entitySpeedY)
    val surfaceId = "surface"
    val surfacePositionX = 10.0
    val surfacePositionY = 10.0
    val surfacePosition = Vector2D(surfacePositionX, surfacePositionY)
    val surfaceRadius = 1.0
    val surfaceForceX = 10.0
    val surfaceForceY = 0.0
    val surfaceForce = Vector2D(surfaceForceX, surfaceForceY)
    val surfaceFrictionIndex = 0.1

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[SurfaceDetection[TestDetector]]

    val entity = makeMovingEntity(id = entityId, position = entityPosition, speed = entitySpeed)
    val surface = makeSurface(id = surfaceId, position = surfacePosition, radius = surfaceRadius)
      .withAppliedForce(surfaceForce).value
      .withFrictionIndex(surfaceFrictionIndex).value

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity.id -> entity))
      .once()

    (mockState.getSurfaces(_: TestScene))
      .expects(InitialScene)
      .returning(Map(surface.id -> surface))
      .once()

    (mockDetection.isInside(_: TestDetector, _: Entity, _: Surface))
      .expects(*, entity, surface)
      .returning(false)
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should not update an entity when surface has no force and no friction"):
    val entityId = "entity"
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entitySpeedX = 1.0
    val entitySpeedY = 1.0
    val entitySpeed = Vector2D(entitySpeedX, entitySpeedY)
    val surfaceId = "surface"
    val surfacePositionX = 0.0
    val surfacePositionY = 0.0
    val surfacePosition = Vector2D(surfacePositionX, surfacePositionY)
    val surfaceRadius = 1.0

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[SurfaceDetection[TestDetector]]

    val entity = makeMovingEntity(id = entityId, position = entityPosition, speed = entitySpeed)
    val surface = makeSurface(id = surfaceId, position = surfacePosition, radius = surfaceRadius)

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity.id -> entity))
      .once()

    (mockState.getSurfaces(_: TestScene))
      .expects(InitialScene)
      .returning(Map(surface.id -> surface))
      .once()

    (mockDetection.isInside(_: TestDetector, _: Entity, _: Surface))
      .expects(*, entity, surface)
      .returning(true)
      .once()

    given PhysicsState[TestScene] = mockState

    mockState.updateEntity.expects(*, *, *).never()

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe InitialScene

  test("the rule should apply force and friction when entity is inside the surface"):
    val entityId = "entity"
    val entityPositionX = 0.0
    val entityPositionY = 0.0
    val entityPosition = Vector2D(entityPositionX, entityPositionY)
    val entitySpeedX = 2.0
    val entitySpeedY = 0.0
    val entitySpeed = Vector2D(entitySpeedX, entitySpeedY)
    val entityWeight = 10
    val surfaceId = "surface"
    val surfacePositionX = 0.0
    val surfacePositionY = 0.0
    val surfacePosition = Vector2D(surfacePositionX, surfacePositionY)
    val surfaceRadius = 5.0
    val surfaceForceX = 10.0
    val surfaceForceY = 0.0
    val surfaceForce = Vector2D(surfaceForceX, surfaceForceY)
    val surfaceFrictionIndex = 0.1
    val finalScene = mock[TestScene]

    val mockState = mock[PhysicsState[TestScene]]
    val mockDetection = summon[SurfaceDetection[TestDetector]]

    val entity = addWeight(
      makeMovingEntity(id = entityId, position = entityPosition, speed = entitySpeed),
      weight = entityWeight
    )
    val surface = makeSurface(id = surfaceId, position = surfacePosition, radius = surfaceRadius)
              .withAppliedForce(surfaceForce).value
              .withFrictionIndex(surfaceFrictionIndex).value

    (mockState.getEntities(_: TestScene))
      .expects(InitialScene)
      .returning(Map(entity.id -> entity))
      .once()

    (mockState.getSurfaces(_: TestScene))
      .expects(InitialScene)
      .returning(Map(surface.id -> surface))
      .once()

    (mockDetection.isInside(_: TestDetector, _: Entity, _: Surface))
      .expects(*, entity, surface)
      .returning(true)
      .once()

    var caughtEntity: Option[Entity] = None

    (mockState.updateEntity(_: TestScene, _: LocatableId, _: Entity))
      .expects(InitialScene, entity.id, *)
      .onCall: (_, _, updatedEntity) =>
        caughtEntity = Some(updatedEntity)
        finalScene

    given PhysicsState[TestScene] = mockState

    val rule = summon[PhysicsRule[TestScene, TestDetector]]
    val result = rule.apply(InitialScene)(using summon[TestDetector], DeltaTimeOneSecond).value

    result shouldBe finalScene
    caughtEntity.value.speed.isDefined shouldBe true
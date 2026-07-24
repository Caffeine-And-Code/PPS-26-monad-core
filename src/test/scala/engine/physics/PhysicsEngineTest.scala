package engine.physics

import engine.core.Scene
import engine.model.{Entity, Surface, Vector2D, add, times}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsEngineTest extends AnyFunSuite with Matchers with MockFactory :

  val CurrentNanoTime = 1_000_000_000L
  val NanoInSeconds = 1_000_000_000.0
  val CurrentSecondTime: Double = CurrentNanoTime.toDouble / NanoInSeconds
  val MockScene: Scene = mock[Scene]

  test("step should be NegativeDeltaTime when delta time is negative") :

    val physics = PhysicsEngine
    val invalidCurrentTime = -1L
    val negativeDeltaError = NegativeDeltaTime(invalidCurrentTime)

    negativeDeltaError shouldBe NegativeDeltaTime(invalidCurrentTime)

  test("step should return the same scene unmodified if the scene is empty") :
    val physics = PhysicsEngine
    val resultScene = physics.step(MockScene, CurrentNanoTime)

    resultScene shouldBe MockScene
//
//  test("step should return the same scene unmodified if time is zero"):
//
//    val zeroTime = 0L
//
//    val entityId = "entity"
//    val entityPosition = Vector2D(10, 10)
//    val entityRadius = 5.0
//    val entitySpeed = Vector2D(100, 100)
//
//    val entity = Entity.circle(entityId, entityPosition, entityRadius)
//      .flatMap(_.withSpeed(entitySpeed))
//      .getOrElse(fail("Error creating an entity"))
//
//    val initialScene = Scene(
//      entities = Map(entity.id -> entity)
//    )
//
//    val resultScene = Physics.step(initialScene, zeroTime)
//
//    resultScene shouldBe initialScene
//
//  test("a surface should apply its force to the entity inside it updating speed and position") :
//
//    val entityId = "player"
//    val entityPosition = Vector2D(0, 0)
//    val entityRadius = 5.0
//    val entityWeight = 10
//    val entitySpeed = Vector2D(0, 0)
//    val surfaceId = "surface"
//    val surfacePosition = Vector2D(0, 0)
//    val surfaceRadius = 20.0
//    val surfaceForce = Vector2D(100, 0)
//
//    val expectedAcceleration = surfaceForce times (1.0 / entityWeight.toDouble)
//    val expectedSpeed = entitySpeed add (expectedAcceleration times CurrentSecondTime)
//    val expectedDisplacement = expectedSpeed times CurrentSecondTime
//    val expectedPosition = entityPosition add expectedDisplacement
//
//    val entity = Entity.circle(entityId, entityPosition, entityRadius)
//      .flatMap(_.withWeight(entityWeight))
//      .flatMap(_.withSpeed(entitySpeed))
//      .getOrElse(fail("Error creating the entity"))
//
//    val surface = Surface.circle(surfaceId, surfacePosition, surfaceRadius)
//      .flatMap(_.withAppliedForce(surfaceForce))
//      .getOrElse(fail("Error creating the surface"))
//
//    val initialScene = Scene(
//      entities = Map(entity.id -> entity),
//      surfaces = Map(surface.id -> surface)
//    )
//
//    val finalScene = Physics.step(initialScene, CurrentNanoTime)
//    val updatedEntity = finalScene.entities(entity.id)
//
//    updatedEntity.speed.get shouldBe expectedSpeed
//    updatedEntity.position shouldBe expectedPosition
//
//  test("a fixed entity should not move or change speed even if a surface applies a force"):
//
//    val entityId = "fixed-player"
//    val entityPosition = Vector2D(0, 0)
//    val entityRadius = 5.0
//    val entityWeight = 10
//    val surfaceId = "surface"
//    val surfacePosition = Vector2D(0, 0)
//    val surfaceRadius = 20.0
//    val surfaceForce = Vector2D(100, 50)
//
//    val entity = Entity.circle(entityId, entityPosition, entityRadius)
//      .flatMap(_.withWeight(entityWeight))
//      .getOrElse(fail("Error creating the entity"))
//
//    val surface = Surface.circle(surfaceId, surfacePosition, surfaceRadius)
//      .flatMap(_.withAppliedForce(surfaceForce))
//      .getOrElse(fail("Error creating the surface"))
//
//    val initialScene = Scene(
//      entities = Map(entity.id -> entity),
//      surfaces = Map(surface.id -> surface)
//    )
//
//    val resultScene = Physics.step(initialScene, CurrentNanoTime)
//    val updatedEntity = resultScene.entities(entity.id)
//
//    updatedEntity.position shouldBe entityPosition
//    updatedEntity.speed shouldBe None
//    updatedEntity.isFixed shouldBe true

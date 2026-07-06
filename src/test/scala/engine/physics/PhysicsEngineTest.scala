package engine.physics

import engine.core.Scene
import engine.model.{Entity, Surface, Vector2D, add, times}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsEngineTest extends AnyFunSuite with Matchers with MockFactory :

  val Physics = PhysicsEngine()
  val CurrentNanoTime = 1_000_000_000L
  val EmptyScene = Scene()

  test("step should throw IllegalArgumentException when time is negative") :

    val invalidCurrentTime = -1L

    val exception = intercept[IllegalArgumentException] :
      Physics.step(EmptyScene, invalidCurrentTime)

    exception shouldNot be(null)

  test("step should return the same scene unmodified if the scene is empty") :

    val resultScene = Physics.step(EmptyScene, CurrentNanoTime)

    resultScene shouldBe EmptyScene

  test("step should return the same scene unmodified if time is zero"):

    val zeroTime = 0L

    val entityId = "entity"
    val entityPosition = Vector2D(10, 10)
    val entityRadius = 5.0
    val entitySpeed = Vector2D(100, 100)

    val entity = Entity.circle(entityId, entityPosition, entityRadius)
      .flatMap(_.withSpeed(entitySpeed))
      .getOrElse(fail("Error creating an entity"))

    val initialScene = Scene(
      entities = Map(entity.id -> entity)
    )

    val resultScene = Physics.step(initialScene, zeroTime)

    resultScene shouldBe initialScene

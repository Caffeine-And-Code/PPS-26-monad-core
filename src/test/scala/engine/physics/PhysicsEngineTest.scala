package engine.physics

import engine.core.Scene
import engine.model.{Entity, Surface, Vector2D, add, times}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PhysicsEngineTest extends AnyFunSuite with Matchers with MockFactory :

  val Physics = PhysicsEngine()
  val EmptyScene = Scene()

  test("step should throw IllegalArgumentException when time is negative") :

    val invalidCurrentTime = -1L

    val exception = intercept[IllegalArgumentException] :
      Physics.step(EmptyScene, invalidCurrentTime)

    exception shouldNot be(null)
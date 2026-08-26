package monad_core.simulator.infrastructure.ai

import monad_core.engine.model.*
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.{renderEntity, renderSurface}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Langchain4jToolResponseTest extends AnyFunSuite with Matchers:

  private val id           = "locatable"
  private val position     = Vector2D(2.0, 6.0)
  private val radius       = 3.0
  private val rotation     = 45.0
  private val angularSpeed = -30.0
  private val damage       = 7

  test("renderEntity should include rotation"):
    val entity       = Entity.circle(id, position, radius, rotation).value
    val expectedLine = s"rotation: $rotation"

    val result = renderEntity(entity)

    result.linesIterator.toList should contain(expectedLine)

  test("renderEntity should include angular speed"):
    val entity       = Entity.circle(id, position, radius).value.withAngularSpeed(angularSpeed)
    val expectedLine = s"angularSpeed: $angularSpeed"

    val result = renderEntity(entity)

    result.linesIterator.toList should contain(expectedLine)

  test("renderEntity should include damage"):
    val entity       = Entity.circle(id, position, radius).flatMap(_.withDamage(damage)).value
    val expectedLine = s"damage: $damage"

    val result = renderEntity(entity)

    result.linesIterator.toList should contain(expectedLine)

  test("renderSurface should include rotation"):
    val surface      = Surface.circle(id, position, radius, rotation).value
    val expectedLine = s"rotation: $rotation"

    val result = renderSurface(surface)

    result.linesIterator.toList should contain(expectedLine)

  test("renderSurface should include damage over time"):
    val surface = Surface
      .circle(id, position, radius)
      .flatMap(_.withDamageOverTime(damage))
      .value
    val expectedLine = s"damageOverTime: $damage"

    val result = renderSurface(surface)

    result.linesIterator.toList should contain(expectedLine)

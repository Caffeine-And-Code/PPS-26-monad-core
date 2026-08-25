package monad_core.performance.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityCountTest extends AnyFunSuite with Matchers:

  test("an entity count can be created from a positive value"):
    val value = 10

    val result = EntityCount.from(value)

    result.map(_.value) shouldBe Right(value)

  test("an entity count cannot be created from a zero value"):
    val value = 0

    val result = EntityCount.from(value)

    result shouldBe Left(InvalidEntityCount(value))

  test("an entity count cannot be created from a negative value"):
    val value = -1

    val result = EntityCount.from(value)

    result shouldBe Left(InvalidEntityCount(value))

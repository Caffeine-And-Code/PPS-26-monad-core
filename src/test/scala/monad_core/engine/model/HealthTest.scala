package monad_core.engine.model

import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HealthTest extends AnyFunSuite with Matchers:

  test("can create an optional health from a valid value"):
    val validHealth = Some(10)

    val health = Health.fromOption(validHealth)

    health.value.map(_.value) shouldBe validHealth

  test("cannot create an optional health from an invalid value"):
    val invalidHealth = Some(0)

    val health = Health.fromOption(invalidHealth)

    health shouldBe Left(HealthCannotBeNegativeOrZero(invalidHealth.value))

  test("applying zero damage leaves health unchanged"):
    val health = Health(10).value

    val result = health - 0

    result.value.value shouldBe 10

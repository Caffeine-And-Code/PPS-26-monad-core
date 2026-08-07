package monad_core.engine.model

import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HealthTest extends AnyFunSuite with Matchers:

  test("applying zero damage leaves health unchanged"):
    val health = Health(10).value

    val result = health - 0

    result.value.value shouldBe 10

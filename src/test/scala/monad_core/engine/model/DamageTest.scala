package monad_core.engine.model

import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DamageTest extends AnyFunSuite with Matchers:

  test("can create an optional damage from a valid value"):
    val validDamage = Some(5)

    val damage = Damage.fromOption(validDamage)

    damage.value.map(_.value) shouldBe validDamage

  test("cannot create an optional damage from an invalid value"):
    val invalidDamage = Some(-1)

    val damage = Damage.fromOption(invalidDamage)

    damage shouldBe Left(DamageCannotBeNegative())

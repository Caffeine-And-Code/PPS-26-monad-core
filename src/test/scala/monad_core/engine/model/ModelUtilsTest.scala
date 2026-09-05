package monad_core.engine.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ModelUtilsTest extends AnyFunSuite with Matchers:

  test("can optionalize an existing valid value"):
    val validDamage = 5

    val optionalDamage = ModelUtils.optionalize(Some(validDamage), Damage(_))

    optionalDamage shouldBe Right(Some(validDamage))

  test("cannot optionalize an existing invalid value"):
    val invalidDamage = -1

    val optionalDamage = ModelUtils.optionalize(Some(invalidDamage), Damage(_))

    optionalDamage shouldBe Left(DamageCannotBeNegative())

  test("can optionalize an empty value"):
    val emptyDamage: Option[Int] = None

    val optionalDamage = ModelUtils.optionalize(emptyDamage, Damage(_))

    optionalDamage shouldBe Right(None)

package engine.core

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LensTest extends AnyFunSuite with Matchers:
  private case class User(age: Int, name: String = "TestName")

  private val ageLens: Lens[User,Int] = Lens(
    get = _.age,
    set = (user, newAge) => user.copy(age = newAge)
  )

  test("A lens can be used to retrieve it's designated data correctly"):
    val expectedAge = 20
    val user = User(age = expectedAge)

    val result = ageLens.get(user)

    result should be(expectedAge)

  test("A lens can be used to update it's designated data correctly"):
    val expectedAgeAfterUpdate = 30
    val user = User(age = 20)

    val userAfterUpdate = ageLens.set(user, expectedAgeAfterUpdate)

    userAfterUpdate.age should be(expectedAgeAfterUpdate)

  test("To a lens a function A can be applied to change it's designated data as the function A intend"):
    val functionA = (age: Int) => age + 5
    val user = User(age = 20)

    val userAfterModify = ageLens.modify(user)(functionA)

    userAfterModify.age should be(functionA(user.age))
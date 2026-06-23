import org.scalatest.flatspec.AnyFlatSpec

import org.scalatest.matchers.should.Matchers

class ExampleTest extends AnyFlatSpec with Matchers {

  "true " should "be different than false" in {
    true shouldNot be(false)
  }
}
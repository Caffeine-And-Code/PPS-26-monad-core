package engine.geometry

import engine.geometry.*
import engine.geometry.Contains.contains
import engine.model.Shape2D.*
import engine.model.{Shape2D, Vector2D}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class ContainsTest extends AnyFunSuite with Inside with Matchers with MockFactory {

  test("the contains method of a Placed entity uses a given Contains implementation"):
    val cases = Table(
      ("placedPosition", "vector", "containsResult"),
      (Vector2D(0, 0), Vector2D(30, 30), true),
      (Vector2D(5, 5), Vector2D(10, 10), false)
    )

    forAll(cases): (position, vector, expected) =>
      val circle = Shape2D.circle(10).value
      val placed: Placed[Circle] = Placed(position, circle)

      val containsInstance = mock[Contains[Circle]]

      containsInstance.checkIfContains
        .expects(placed, vector)
        .returning(expected)
        .once()

      val result = placed.contains(vector)(using containsInstance)

      result shouldBe expected
}

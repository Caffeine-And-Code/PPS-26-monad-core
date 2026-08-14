package monad_core.simulator.presentation.components

import monad_core.simulator.presentation.components.IconButton
import org.scalamock.function.MockFunction1
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.TableFor1
import org.scalatest.prop.Tables.Table

class IconButtonTest extends AnyFunSuite with Inside with Matchers with MockFactory:

  val iterations: TableFor1[Boolean] = Table(
    "currentIsActive",
    true,
    false
  )

  def arrangeBaseInputs(currentIsActive: Boolean): (MockFunction1[Boolean, Unit], Boolean) =
    val mockedOnClick       = mockFunction[Boolean, Unit]
    val expectedNewIsActive = !currentIsActive

    mockedOnClick.expects(expectedNewIsActive).once()

    (mockedOnClick, expectedNewIsActive)

  test("toggleIsActive with default values correctly toggles the currentValue and returns it"):
    forAll(iterations): currentIsActive =>
      val (mockedOnClick, expectedNewIsActive) = arrangeBaseInputs(currentIsActive)

      val result = IconButton.toggleIsActive(currentIsActive, mockedOnClick)

      result should be(expectedNewIsActive)

  test("toggleIsActive correctly calls an internal on click function"):
    forAll(iterations): currentIsActive =>
      val (mockedOnClick, expectedNewIsActive) = arrangeBaseInputs(currentIsActive)
      val internalMockedOnClick                = mockFunction[Boolean, Unit]

      internalMockedOnClick.expects(expectedNewIsActive).once()

      val result = IconButton.toggleIsActive(currentIsActive, mockedOnClick, internalMockedOnClick)

      result should be(expectedNewIsActive)

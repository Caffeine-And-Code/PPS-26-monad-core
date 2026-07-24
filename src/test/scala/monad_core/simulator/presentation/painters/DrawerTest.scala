package monad_core.simulator.presentation.painters

import monad_core.engine.model.{Entity, Shape2D, TeamId, Vector2D}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.{BeforeAndAfterEach, Inside}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.scene.paint.Color
import scala.util.Random

class DrawerTest extends AnyFunSuite with Matchers with Inside with MockFactory with BeforeAndAfterEach:
  val CircleEntity: Entity = Entity.circle("CircleId", Vector2D(0, 0), 1).value
  val RectangleEntity: Entity = Entity.rectangle("RectangleId", Vector2D(0, 0), 10, 10).value

  override def afterEach(): Unit =
    Drawer.buffer.clear()

  def generateRandomTeamId(): TeamId =
    TeamId(Random.nextString(5)).value

  test("DrawCircle, provided with a Circle Locatable, enlist it in the drawing buffer"):
    val expectedCommandInList = CircleEntity.shape match
      case Shape2D.Circle(r) =>
        DrawCommand.Circle(
          CircleEntity.position.x,
          CircleEntity.position.y,
          r,
          Drawer.baseColor
        )
      case _ => fail("CircleEntity is not a Circle")

    Drawer.drawCircle(CircleEntity, Drawer.baseColor)

    Drawer.getBuffer.toList.length should be(1)
    Drawer.getBuffer.toList.head should be(expectedCommandInList)

  test("DrawCircle, provided with a Rectangle Locatable, does nothing"):
    Drawer.drawCircle(RectangleEntity, Drawer.baseColor)

    Drawer.getBuffer.toList.length should be(0)

  test("DrawRectangle, provided with a Rectangle Locatable, enlist it in the drawing buffer"):
    val expectedCommandInList = RectangleEntity.shape match
      case Shape2D.Rectangle(w, h) =>
        DrawCommand.Rectangle(
          RectangleEntity.position.x,
          RectangleEntity.position.y,
          w,
          h,
          Drawer.baseColor
        )
      case _ => fail("RectangleEntity is not a Rectangle")

    Drawer.drawRectangle(RectangleEntity, Drawer.baseColor)

    Drawer.getBuffer.toList.length should be(1)
    Drawer.getBuffer.toList.head should be(expectedCommandInList)

  test("DrawRectangle, provided with a Circle Locatable, does nothing"):
    Drawer.drawRectangle(CircleEntity, Drawer.baseColor)

    Drawer.getBuffer.toList.length should be(0)

  test("A Drawer can generate a random color given a TeamId"):
    val teamIds = Table(
      "teamId",
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
    )

    forAll(teamIds): teamId =>
      val color = Drawer.teamIdColorRelation(teamId)

      color shouldBe a[Color]

  test("Similar TeamId gets different colors"):

    def createSimilarString(original: String): String =
      val newHead = if (original.head == 'A') 'B' else 'A'
      original.updated(0, newHead)

    def colorDistance(c1: Color, c2: Color): Double =
      val dr = c1.red - c2.red
      val dg = c1.green - c2.green
      val db = c1.blue - c2.blue

      Math.sqrt(0.30 * dr * dr + 0.59 * dg * dg + 0.11 * db * db)

    def areColorsDifferentEnough(c1: Color, c2: Color, threshold: Double = 0.3): Boolean =
      colorDistance(c1, c2) >= threshold

    val firstTeamId = generateRandomTeamId()
    val secondTeamId = TeamId(createSimilarString(firstTeamId.value)).value

    val firstColor = Drawer.teamIdColorRelation(firstTeamId)
    val secondColor = Drawer.teamIdColorRelation(secondTeamId)

    areColorsDifferentEnough(firstColor, secondColor) should be(true)
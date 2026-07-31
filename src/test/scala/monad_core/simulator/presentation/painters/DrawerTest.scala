package monad_core.simulator.presentation.painters

import monad_core.engine.model.{Entity, Shape2D, TeamId, Vector2D}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.{BeforeAndAfterEach, Inside}
import scalafx.scene.paint.Color

import scala.util.Random

class DrawerTest extends AnyFunSuite with Matchers with Inside with MockFactory with BeforeAndAfterEach:
  val CircleEntity: Entity = Entity.circle("CircleId", Vector2D(0, 0), 1).value
  val RectangleEntity: Entity = Entity.rectangle("RectangleId", Vector2D(0, 0), 10, 10).value

  override def beforeEach(): Unit =
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

  test("teamIdColorRelation is deterministic"):
    val teamId = generateRandomTeamId()
    val color1 = Drawer.teamIdColorRelation(teamId)
    val color2 = Drawer.teamIdColorRelation(teamId)

    color1 should be(color2)

  test("Different TeamIds get different colors"):
    val firstTeamId = TeamId("TeamA").value
    val secondTeamId = TeamId("TeamB").value

    val firstColor = Drawer.teamIdColorRelation(firstTeamId)
    val secondColor = Drawer.teamIdColorRelation(secondTeamId)

    firstColor should not be secondColor
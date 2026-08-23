package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.simulator.application.engine.DrawCommand
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.{BeforeAndAfterEach, Inside}

import scala.util.Random

class PaintArchitectTest
    extends AnyFunSuite
    with Matchers
    with Inside
    with MockFactory
    with BeforeAndAfterEach:
  val CircleEntity: Entity    = Entity.circle("CircleId", Vector2D(0, 0), 1).value
  val RectangleEntity: Entity = Entity.rectangle("RectangleId", Vector2D(0, 0), 10, 10).value
  val ArchitectEntityBaseColor: EngineColor = PaintArchitect.baseEntityColor.value

  override def beforeEach(): Unit =
    PaintArchitect.drainBuffer()

  override def afterEach(): Unit =
    PaintArchitect.drainBuffer()

  def generateRandomTeamId(): TeamId =
    TeamId(Random.nextString(5)).value

  test("DrawCircle, provided with a Circle Locatable, enlists it in the drawing buffer"):
    val expectedCommandInList = CircleEntity.shape match
      case Shape2D.Circle(r) =>
        DrawCommand.Circle(
          CircleEntity.position.x,
          CircleEntity.position.y,
          r,
          ArchitectEntityBaseColor
        )
      case _ => fail("CircleEntity is not a Circle")

    PaintArchitect.drawCircle(CircleEntity, ArchitectEntityBaseColor)

    val commands = PaintArchitect.drainBuffer()
    commands.length should be(1)
    commands.head should be(expectedCommandInList)

  test("DrawCircle, provided with a Rectangle Locatable, does nothing"):
    PaintArchitect.drawCircle(RectangleEntity, ArchitectEntityBaseColor)

    PaintArchitect.drainBuffer().length should be(0)

  test("DrawRectangle, provided with a Rectangle Locatable, enlists it in the drawing buffer"):
    val expectedCommandInList = RectangleEntity.shape match
      case Shape2D.Rectangle(w, h) =>
        DrawCommand.Rectangle(
          RectangleEntity.position.x,
          RectangleEntity.position.y,
          h,
          w,
          RectangleEntity.rotation,
          ArchitectEntityBaseColor
        )
      case _ => fail("RectangleEntity is not a Rectangle")

    PaintArchitect.drawRectangle(RectangleEntity, ArchitectEntityBaseColor)

    val commands = PaintArchitect.drainBuffer()
    commands.length should be(1)
    commands.head should be(expectedCommandInList)

  test("DrawRectangle, provided with a Circle Locatable, does nothing"):
    PaintArchitect.drawRectangle(CircleEntity, ArchitectEntityBaseColor)

    PaintArchitect.drainBuffer().length should be(0)

  test("DrawRectangle preserves the locatable rotation in the draw command"):
    val rotated = RectangleEntity.rotateTo(45.0).value

    PaintArchitect.drawRectangle(rotated, ArchitectEntityBaseColor)

    inside(PaintArchitect.drainBuffer().head):
      case DrawCommand.Rectangle(_, _, _, _, rotation, _) => rotation shouldBe 45.0

  test("drainBuffer returns accumulated commands and clears internal state"):
    PaintArchitect.drawCircle(CircleEntity, ArchitectEntityBaseColor)
    PaintArchitect.drawRectangle(RectangleEntity, ArchitectEntityBaseColor)

    val extractedCommands = PaintArchitect.drainBuffer()
    extractedCommands.length should be(2)

    PaintArchitect.drainBuffer().length should be(0)

  test("A PaintArchitect can generate a random color given a TeamId"):
    val teamIds = Table(
      "teamId",
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId(),
      generateRandomTeamId()
    )

    forAll(teamIds): teamId =>
      val result = PaintArchitect.teamIdColorRelation(teamId)

      inside(result):
        case Right(color) => color shouldBe a[EngineColor]

  test("teamIdColorRelation is deterministic"):
    val teamId = generateRandomTeamId()
    val color1 = PaintArchitect.teamIdColorRelation(teamId)
    val color2 = PaintArchitect.teamIdColorRelation(teamId)

    color1 should be(color2)

  test("Different TeamIds get different colors"):
    val firstTeamId  = TeamId("TeamA").value
    val secondTeamId = TeamId("TeamB").value

    val firstColor  = PaintArchitect.teamIdColorRelation(firstTeamId)
    val secondColor = PaintArchitect.teamIdColorRelation(secondTeamId)

    firstColor should not be secondColor

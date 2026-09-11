package monad_core.simulator.infrastructure.engine.painters

import monad_core.engine.model.*
import monad_core.engine.simulator.DrawCommand
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.Inside

import scala.util.Random

class PaintArchitectTest extends AnyFunSuite with Matchers with Inside:
  val CircleEntity: Entity    = Entity.circle("CircleId", Vector2D(0, 0), 1).value
  val RectangleEntity: Entity = Entity.rectangle("RectangleId", Vector2D(0, 0), 10, 10).value
  val ArchitectEntityBaseColor: EngineColor = PaintArchitect.baseEntityColor.value

  def generateRandomTeamId(): TeamId =
    TeamId(Random.nextString(5)).value

  test("drawCircle should describe a circle with an immutable command"):
    val expectedCommand = DrawCommand.Circle(
      CircleEntity.position.x,
      CircleEntity.position.y,
      1,
      ArchitectEntityBaseColor
    )

    PaintArchitect.drawCircle(CircleEntity, ArchitectEntityBaseColor) shouldBe Some(expectedCommand)

  test("drawCircle should return None for a rectangle"):
    PaintArchitect.drawCircle(RectangleEntity, ArchitectEntityBaseColor) shouldBe None

  test("drawRectangle should describe a rectangle with an immutable command"):
    val expectedCommand = DrawCommand.Rectangle(
      RectangleEntity.position.x,
      RectangleEntity.position.y,
      10,
      10,
      RectangleEntity.rotation,
      ArchitectEntityBaseColor
    )

    PaintArchitect.drawRectangle(RectangleEntity, ArchitectEntityBaseColor) shouldBe
      Some(expectedCommand)

  test("drawRectangle should return None for a circle"):
    PaintArchitect.drawRectangle(CircleEntity, ArchitectEntityBaseColor) shouldBe None

  test("drawRectangle should preserve the locatable rotation"):
    val rotated = RectangleEntity.rotateTo(45.0).value

    inside(PaintArchitect.drawRectangle(rotated, ArchitectEntityBaseColor).value):
      case DrawCommand.Rectangle(_, _, _, _, rotation, _) => rotation shouldBe 45.0

  test("drawing the same entity twice should return the same value"):
    val first  = PaintArchitect.drawCircle(CircleEntity, ArchitectEntityBaseColor)
    val second = PaintArchitect.drawCircle(CircleEntity, ArchitectEntityBaseColor)

    first shouldBe second

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

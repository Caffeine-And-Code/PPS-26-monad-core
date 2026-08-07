package monad_core.simulator.presentation.components.forms

import helpers.arrangers.MonadCoreTeamArranger
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreTeam}
import monad_core.simulator.presentation.components.forms.base.{SelectFieldSpec, TextFieldSpec}
import monad_core.simulator.presentation.components.forms.parsers.{BaseFormParser, EntityFormParser, LocatableFormShapes}
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SaveEntityFormDialogTest extends AnyFunSuite with Inside with Matchers:

  private val Id: String = "id"
  private val Position: (Double, Double) = (1, 2)
  private val Circle: SimulationCircle = SimulationCircle(5.0)
  private val Rectangle: SimulationRectangle = SimulationRectangle(7.0, 6.0)
  private val Speed: (Double, Double) = (3, 4)
  private val Weight: Int = 10
  private val Health: Int = 20
  private val TeamId: String = "teamIdValue"

  private def circleEntity: MonadCoreEntity = MonadCoreEntity(Id, Position, Circle)

  private def rectangleEntity: MonadCoreEntity = MonadCoreEntity(Id, Position, Rectangle)

  private val teams: Seq[MonadCoreTeam] = MonadCoreTeamArranger.arrangeTeams

  private def completeEntity(entity: MonadCoreEntity): MonadCoreEntity =
    MonadCoreEntity(
      entity.id,
      entity.position,
      entity.shape,
      speed = Some(Speed),
      health = Some(Health),
      weight = Some(Weight),
      teamId = Some(TeamId)
    )

  test("buildDefaultValues should return empty defaults when no entity is provided"):
    val result = SaveEntityFormDialog.buildDefaultValues(None)

    result should be(SaveEntityFormDefaultValues())

  test("buildDefaultValues should map a circle entity's basic fields"):
    val entity = circleEntity

    val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

    result.x should be(Some(Position._1.toString))
    result.y should be(Some(Position._2.toString))
    result.shape should be(Some(LocatableFormShapes.CircleLabel))
    result.radius should be(Some(Circle.radius.toString))
    result.height should be(None)
    result.length should be(None)

  test("buildDefaultValues should map a rectangle entity's basic fields"):
    val entity = rectangleEntity

    val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

    result.x should be(Some(Position._1.toString))
    result.y should be(Some(Position._2.toString))
    result.shape should be(Some(LocatableFormShapes.RectangleLabel))
    result.radius should be(None)
    result.height should be(Some(Rectangle.height.toString))
    result.length should be(Some(Rectangle.width.toString))

  test("buildDefaultValues should leave optional fields empty when entity doesn't have them set"):
    val entity = circleEntity

    val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

    result.teamId should be(None)
    result.weight should be(None)
    result.health should be(None)
    result.speedX should be(None)
    result.speedY should be(None)

  test("buildDefaultValues should map optional fields when entity has them set"):
    val cases = Table(
      "entity",
      circleEntity,
      rectangleEntity
    )

    forAll(cases): baseEntity =>
      val entity = completeEntity(baseEntity)

      val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

      result.teamId should be(Some(TeamId))
      result.weight should be(Some(Weight.toString))
      result.health should be(Some(Health.toString))
      result.speedX should be(Some(Speed._1.toString))
      result.speedY should be(Some(Speed._2.toString))

  test("buildFields should build all top-level fields with correct ids"):
    val defaultValues = SaveEntityFormDefaultValues()

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    fields.map(_.id) should be(
      Seq(
        EntityFormParser.PositionXKey,
        EntityFormParser.PositionYKey,
        EntityFormParser.ShapeKey,
        EntityFormParser.SpeedXKey,
        EntityFormParser.SpeedYKey,
        EntityFormParser.WeightKey,
        EntityFormParser.HealthKey,
        EntityFormParser.TeamIdKey
      )
    )

  test("buildFields should propagate default values into the corresponding text fields"):
    val defaultValues = SaveEntityFormDefaultValues(
      x = Some("1.0"),
      y = Some("2.0"),
      speedX = Some("3.0"),
      speedY = Some("4.0"),
      weight = Some("5.0"),
      health = Some("6.0")
    )

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.PositionXKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("1.0"))

    inside(fields.find(_.id == EntityFormParser.PositionYKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("2.0"))

    inside(fields.find(_.id == EntityFormParser.SpeedXKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("3.0"))

    inside(fields.find(_.id == EntityFormParser.SpeedYKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("4.0"))

    inside(fields.find(_.id == EntityFormParser.WeightKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("5.0"))

    inside(fields.find(_.id == EntityFormParser.HealthKey).value):
      case tf: TextFieldSpec => tf.defaultValue should be(Some("6.0"))

  test("buildFields should build the shape field with circle and rectangle dependent fields"):
    val defaultValues = SaveEntityFormDefaultValues()

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.ShapeKey).value):
      case select: SelectFieldSpec =>
        select.options should be(SaveEntityFormDialog.Shapes)

        val circleFields = select.dependentFields(LocatableFormShapes.CircleLabel)
        circleFields.map(_.id) should be(Seq(BaseFormParser.RadiusKey))

        val rectangleFields = select.dependentFields(LocatableFormShapes.RectangleLabel)
        rectangleFields.map(_.id) should be(Seq(BaseFormParser.LengthKey, BaseFormParser.HeightKey))

  test("buildFields should propagate shape-specific default values into dependent fields"):
    val defaultValues = SaveEntityFormDefaultValues(
      shape = Some(LocatableFormShapes.RectangleLabel),
      radius = Some("5.0"),
      height = Some("6.0"),
      length = Some("7.0")
    )

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.ShapeKey).value):
      case select: SelectFieldSpec =>
        select.defaultValue should be(Some(LocatableFormShapes.RectangleLabel))

        inside(select.dependentFields(LocatableFormShapes.CircleLabel).head):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("5.0"))

        inside(select.dependentFields(LocatableFormShapes.RectangleLabel).find(_.id == BaseFormParser.HeightKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("6.0"))

        inside(select.dependentFields(LocatableFormShapes.RectangleLabel).find(_.id == BaseFormParser.LengthKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("7.0"))

  test("buildFields should build the team select field from provided teams and default team id"):
    val defaultValues = SaveEntityFormDefaultValues(teamId = Some("RedTeam"))

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.TeamIdKey).value):
      case select: SelectFieldSpec =>
        select.options should be(teams.map(_.id).appended(""))
        select.defaultValue should be(Some("RedTeam"))

  test("buildFields should return an empty team options list when no teams are provided"):
    val defaultValues = SaveEntityFormDefaultValues()

    val fields = SaveEntityFormDialog.buildFields(Seq.empty, defaultValues)

    inside(fields.find(_.id == EntityFormParser.TeamIdKey).value):
      case select: SelectFieldSpec =>
        select.options should be(Seq(""))
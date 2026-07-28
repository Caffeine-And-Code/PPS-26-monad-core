package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.*
import monad_core.simulator.presentation.components.forms.parsers.{EntityFormParser, EntityShapes}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.Inside
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class SaveEntityFormDialogTest extends AnyFunSuite with Inside with Matchers:

  private val EntityId = "id"
  private val EntityPosition = Vector2D(1, 2)
  private val EntityRadius = 5.0
  private val EntityHeight = 6.0
  private val EntityLength = 7.0
  private val EntitySpeed = Vector2D(3, 4)
  private val EntityWeight = 10
  private val EntityHealth = 20
  private val EntityTeamId = TeamId("teamIdValue").value

  private def circleEntity: Entity = Entity.circle(EntityId, EntityPosition, EntityRadius).value
  private def rectangleEntity: Entity = Entity.rectangle(EntityId, EntityPosition, EntityHeight, EntityLength).value

  private def completeEntity(entity: Entity): Entity =
    val either = for
      withSpeed <- entity.withSpeed(EntitySpeed)
      withHealth <- withSpeed.withHealth(EntityHealth)
      withWeight <- withHealth.withWeight(EntityWeight)
      withTeam <- withWeight.withTeamId(EntityTeamId.value)
    yield withTeam
    either.value
  
  test("buildDefaultValues should return empty defaults when no entity is provided"):
    val result = SaveEntityFormDialog.buildDefaultValues(None)

    result should be(SaveEntityFormDefaultValues())

  test("buildDefaultValues should map a circle entity's basic fields"):
    val entity = circleEntity

    val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

    result.x should be(Some(EntityPosition.x.toString))
    result.y should be(Some(EntityPosition.y.toString))
    result.shape should be(Some(EntityShapes.CircleLabel))
    result.radius should be(Some(EntityRadius.toString))
    result.height should be(None)
    result.length should be(None)

  test("buildDefaultValues should map a rectangle entity's basic fields"):
    val entity = rectangleEntity

    val result = SaveEntityFormDialog.buildDefaultValues(Some(entity))

    result.x should be(Some(EntityPosition.x.toString))
    result.y should be(Some(EntityPosition.y.toString))
    result.shape should be(Some(EntityShapes.RectangleLabel))
    result.radius should be(None)
    result.height should be(Some(EntityHeight.toString))
    result.length should be(Some(EntityLength.toString))

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

      result.teamId should be(Some(EntityTeamId.value))
      result.weight should be(Some(EntityWeight.toString))
      result.health should be(Some(EntityHealth.toString))
      result.speedX should be(Some(EntitySpeed.x.toString))
      result.speedY should be(Some(EntitySpeed.y.toString))
  
  private val teams: Seq[Team] = Seq(
    Team(TeamId("RedTeam").value, Set.empty).value,
    Team(TeamId("BlueTeam").value, Set.empty).value
  )

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

        val circleFields = select.dependentFields(EntityShapes.CircleLabel)
        circleFields.map(_.id) should be(Seq(EntityFormParser.RadiusKey))

        val rectangleFields = select.dependentFields(EntityShapes.RectangleLabel)
        rectangleFields.map(_.id) should be(Seq(EntityFormParser.HeightKey, EntityFormParser.LengthKey))

  test("buildFields should propagate shape-specific default values into dependent fields"):
    val defaultValues = SaveEntityFormDefaultValues(
      shape = Some(EntityShapes.RectangleLabel),
      radius = Some("5.0"),
      height = Some("6.0"),
      length = Some("7.0")
    )

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.ShapeKey).value):
      case select: SelectFieldSpec =>
        select.defaultValue should be(Some(EntityShapes.RectangleLabel))

        inside(select.dependentFields(EntityShapes.CircleLabel).head):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("5.0"))

        inside(select.dependentFields(EntityShapes.RectangleLabel).find(_.id == EntityFormParser.HeightKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("6.0"))

        inside(select.dependentFields(EntityShapes.RectangleLabel).find(_.id == EntityFormParser.LengthKey).value):
          case tf: TextFieldSpec => tf.defaultValue should be(Some("7.0"))

  test("buildFields should build the team select field from provided teams and default team id"):
    val defaultValues = SaveEntityFormDefaultValues(teamId = Some("RedTeam"))

    val fields = SaveEntityFormDialog.buildFields(teams, defaultValues)

    inside(fields.find(_.id == EntityFormParser.TeamIdKey).value):
      case select: SelectFieldSpec =>
        select.options should be(teams.map(_.id.value))
        select.defaultValue should be(Some("RedTeam"))

  test("buildFields should return an empty team options list when no teams are provided"):
    val defaultValues = SaveEntityFormDefaultValues()

    val fields = SaveEntityFormDialog.buildFields(Seq.empty, defaultValues)

    inside(fields.find(_.id == EntityFormParser.TeamIdKey).value):
      case select: SelectFieldSpec => select.options should be(Seq.empty)
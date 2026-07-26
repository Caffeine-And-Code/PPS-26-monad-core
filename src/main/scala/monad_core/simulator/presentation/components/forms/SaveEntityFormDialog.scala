package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Shape2D, Team}
import monad_core.simulator.presentation.components.forms.parsers.EntityShapes.getEnumValue
import monad_core.simulator.presentation.components.forms.parsers.{EntityFormParser, EntityShapes}
import scalafx.stage.Window

final case class SaveEntityFormDialogProps(
                                            title: String,
                                            onSubmit: Entity => Unit,
                                            onError: EngineError => Unit,
                                            teams: Seq[Team],
                                            owner: Option[Window] = None,
                                            entityToUpdate: Option[Entity] = None
                                          )

private case class SaveEntityFormDefaultValues(
                                                x: Option[String] = Option.apply("10.0"),
                                                y: Option[String] = Option.apply("10.0"),
                                                shape: Option[String] = Option.apply(EntityShapes.CircleLabel),
                                                speedX: Option[String] = Option.empty,
                                                speedY: Option[String] = Option.empty,
                                                weight: Option[String] = Option.empty,
                                                health: Option[String] = Option.empty,
                                                teamId: Option[String] = Option.empty,
                                                radius: Option[String] = Option.empty,
                                                height: Option[String] = Option.empty,
                                                length: Option[String] = Option.empty,
                                              )

object SaveEntityFormDialog:
  private val Shapes = Seq(EntityShapes.CircleLabel, EntityShapes.RectangleLabel)

  def show(props: SaveEntityFormDialogProps): Either[EngineError, Unit] = {
    val defaultValues = props.entityToUpdate match
      case None => SaveEntityFormDefaultValues()
      case Some(entity) =>
        val (radius, height, length) = entity.shape match
          case Shape2D.Circle(r) => (Some(r.toString), None, None)
          case Shape2D.Rectangle(h, l) => (None, Some(h.toString), Some(l.toString))

        SaveEntityFormDefaultValues(
          x = Some(entity.position.x.toString),
          y = Some(entity.position.y.toString),
          shape = Some(entity.shape.getEnumValue.getStringValue),
          radius = radius,
          height = height,
          length = length,
          teamId = entity.teamId.map(_.value),
          weight = entity.weight.map(_.toString),
          health = entity.health.map(_.toString),
          speedX = entity.speed.map(_.x.toString),
          speedY = entity.speed.map(_.y.toString)
        )

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = buildFields(props.teams, defaultValues),
        owner = props.owner,
        onSubmit = values =>
          val result = props.entityToUpdate match
            case Some(entity) =>
              EntityFormParser.buildEntity(values, () => entity.id.value)
            case None =>
              EntityFormParser.buildEntity(values)

          result match
            case Right(entity) => props.onSubmit(entity)
            case Left(error) => props.onError(error)
      )
    )
  }

  private def buildFields(teams: Seq[Team], defaultValues: SaveEntityFormDefaultValues): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(id = EntityFormParser.PositionXKey, label = "Initial X Position", defaultValue = defaultValues.x),
      TextFieldSpec(id = EntityFormParser.PositionYKey, label = "Initial Y Position", defaultValue = defaultValues.y),
      SelectFieldSpec(
        id = EntityFormParser.ShapeKey,
        label = "Shape",
        options = Shapes,
        dependentFields = Map(
          EntityShapes.CircleLabel -> Seq(
            TextFieldSpec(id = EntityFormParser.RadiusKey, label = "Radius", defaultValue = defaultValues.radius)
          ),
          EntityShapes.RectangleLabel -> Seq(
            TextFieldSpec(id = EntityFormParser.HeightKey, label = "Width", defaultValue = defaultValues.height),
            TextFieldSpec(id = EntityFormParser.LengthKey, label = "Height", defaultValue = defaultValues.length)
          )
        ),
        defaultValue = defaultValues.shape
      ),
      TextFieldSpec(id = EntityFormParser.SpeedXKey, label = "Initial Speed X", defaultValue = defaultValues.speedX),
      TextFieldSpec(id = EntityFormParser.SpeedYKey, label = "Initial Speed Y", defaultValue = defaultValues.speedY),
      TextFieldSpec(id = EntityFormParser.WeightKey, label = "Weight", defaultValue = defaultValues.weight),
      TextFieldSpec(id = EntityFormParser.HealthKey, label = "Health", defaultValue = defaultValues.health),
      SelectFieldSpec(
        id = EntityFormParser.TeamIdKey,
        label = "Team",
        options = teams.map(_.id.value),
        defaultValue = defaultValues.teamId
      )
    )
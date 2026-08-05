package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Team}
import monad_core.simulator.domain.engine.MonadCoreEntity
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.*
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{getDefaultValuesByShape, getEnumValue}
import monad_core.simulator.presentation.components.forms.parsers.{BaseFormParser, EntityFormParser, LocatableFormShapes}
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

final case class SaveEntityFormDialogProps(
                                            title: String,
                                            onSubmit: MonadCoreEntity => Unit,
                                            onError: BaseError => Unit,
                                            teams: Seq[Team],
                                            anchorNode: Option[Node] = None,
                                            entityToUpdate: Option[MonadCoreEntity] = None
                                          )

private object EntityFormDefaults:
  val InitialX: String = "10.0"
  val InitialY: String = "10.0"
  val InitialShape: String = LocatableFormShapes.CircleLabel

private case class SaveEntityFormDefaultValues(
                                                x: Option[String] = Option.apply(EntityFormDefaults.InitialX),
                                                y: Option[String] = Option.apply(EntityFormDefaults.InitialY),
                                                shape: Option[String] = Option.apply(EntityFormDefaults.InitialShape),
                                                speedX: Option[String] = None,
                                                speedY: Option[String] = None,
                                                weight: Option[String] = None,
                                                health: Option[String] = None,
                                                teamId: Option[String] = None,
                                                radius: Option[String] = None,
                                                height: Option[String] = None,
                                                length: Option[String] = None,
                                              )

object SaveEntityFormDialog:
  private[forms] val Shapes = Seq(LocatableFormShapes.CircleLabel, LocatableFormShapes.RectangleLabel)

  def show(props: SaveEntityFormDialogProps): Either[BaseError, Unit] = {
    val defaultValues = buildDefaultValues(props.entityToUpdate)

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = buildFields(props.teams, defaultValues),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit = values =>
          val result = props.entityToUpdate match
            case Some(entity) =>
              EntityFormParser.buildEntity(values, () => entity.id)
            case None =>
              EntityFormParser.buildEntity(values)

          result.matchToResult(props.onError)(props.onSubmit)
      )
    )
  }

  private[forms] def buildDefaultValues(entityToUpdate: Option[MonadCoreEntity]): SaveEntityFormDefaultValues =
    entityToUpdate match
      case None => SaveEntityFormDefaultValues()
      case Some(entity) =>
        val (radius, width, height) = entity.shape.getDefaultValuesByShape

        SaveEntityFormDefaultValues(
          x = Some(entity.position._1.toString),
          y = Some(entity.position._2.toString),
          shape = Some(entity.shape.getEnumValue.getStringValue),
          radius = radius,
          height = height,
          length = width,
          teamId = entity.teamId,
          weight = entity.weight.map(_.toString),
          health = entity.health.map(_.toString),
          speedX = entity.speed.map(_._1.toString),
          speedY = entity.speed.map(_._2.toString)
        )

  private[forms] def buildFields(teams: Seq[Team], defaultValues: SaveEntityFormDefaultValues): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(id = EntityFormParser.PositionXKey, label = "Initial X Position", defaultValue = defaultValues.x),
      TextFieldSpec(id = EntityFormParser.PositionYKey, label = "Initial Y Position", defaultValue = defaultValues.y),
      SelectFieldSpec(
        id = EntityFormParser.ShapeKey,
        label = "Shape",
        options = Shapes,
        dependentFields = Map(
          LocatableFormShapes.CircleLabel -> Seq(
            TextFieldSpec(id = BaseFormParser.RadiusKey, label = "Radius", defaultValue = defaultValues.radius)
          ),
          LocatableFormShapes.RectangleLabel -> Seq(
            TextFieldSpec(id = BaseFormParser.HeightKey, label = "Width", defaultValue = defaultValues.height),
            TextFieldSpec(id = BaseFormParser.LengthKey, label = "Height", defaultValue = defaultValues.length)
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
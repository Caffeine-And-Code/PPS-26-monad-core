package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.{Entity, Team}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.*
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  getDefaultValuesByShape,
  getEnumValue
}
import monad_core.simulator.presentation.components.forms.parsers.{
  EntityFormParser,
  LocatableFormShapes
}
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

/**
 * Configuration of an entity creation or editing dialog.
 *
 * @param title
 *   title displayed by the dialog
 * @param onSubmit
 *   callback invoked with the validated entity
 * @param onError
 *   callback invoked when submitted values cannot produce an entity
 * @param teams
 *   teams available for assignment
 * @param anchorNode
 *   optional node whose window owns the dialog
 * @param entityToUpdate
 *   existing entity to edit; `None` selects creation mode
 */
final case class SaveEntityFormDialogProps(
    title: String,
    onSubmit: Entity => Unit,
    onError: BaseError => Unit,
    teams: Seq[Team],
    anchorNode: Option[Node] = None,
    entityToUpdate: Option[Entity] = None
)

private object EntityFormDefaults:
  val InitialX: String     = "10.0"
  val InitialY: String     = "10.0"
  val InitialShape: String = LocatableFormShapes.CircleLabel

private case class SaveEntityFormDefaultValues(
    x: Option[String] = Option.apply(EntityFormDefaults.InitialX),
    y: Option[String] = Option.apply(EntityFormDefaults.InitialY),
    shape: Option[String] = Option.apply(EntityFormDefaults.InitialShape),
    speedX: Option[String] = None,
    speedY: Option[String] = None,
    rotation: Option[String] = Some("0.0"),
    angularSpeed: Option[String] = None,
    weight: Option[String] = None,
    health: Option[String] = None,
    damage: Option[String] = None,
    teamId: Option[String] = None,
    radius: Option[String] = None,
    height: Option[String] = None,
    length: Option[String] = None
)

/** Builds entity forms and converts their submitted values into engine entities. */
object SaveEntityFormDialog:

  /** Shape labels offered by the entity form. */
  private[forms] val Shapes =
    Seq(LocatableFormShapes.CircleLabel, LocatableFormShapes.RectangleLabel)

  private case class SaveEntityViewModel(entityToUpdate: Option[Entity])

  extension (viewModel: SaveEntityViewModel)

    private def resolveEntity(values: Map[String, String]): Either[BaseError, Entity] =
      viewModel.entityToUpdate match
        case Some(entity) => EntityFormParser.buildEntity(values, () => entity.id.value)
        case None         => EntityFormParser.buildEntity(values)

  /**
   * Displays an entity creation or editing dialog.
   *
   * In editing mode the existing identifier is preserved. Parsing and domain errors produced on submission are sent
   * to `props.onError`; this method returns failures raised while constructing the dialog itself.
   *
   * @param props
   *   dialog mode, available teams and result callbacks
   * @return
   *   the result of building and displaying the underlying form dialog
   */
  def show(props: SaveEntityFormDialogProps): Either[BaseError, Unit] = {
    val defaultValues = buildDefaultValues(props.entityToUpdate)
    val viewModel     = SaveEntityViewModel(props.entityToUpdate)

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = buildFields(props.teams, defaultValues),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit =
          values => viewModel.resolveEntity(values).matchToResult(props.onError)(props.onSubmit)
      )
    )
  }

  /**
   * Derives field defaults for creation or editing mode.
   *
   * @param entityToUpdate
   *   entity whose current properties populate the fields
   * @return
   *   creation defaults when absent, otherwise textual values extracted from the entity
   */
  private[forms] def buildDefaultValues(
      entityToUpdate: Option[Entity]
  ): SaveEntityFormDefaultValues =
    entityToUpdate match
      case None => SaveEntityFormDefaultValues()
      case Some(entity) =>
        val (radius, height, length) = entity.shape.getDefaultValuesByShape

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
          damage = entity.damage.map(_.value.toString),
          speedX = entity.speed.map(_.x.toString),
          speedY = entity.speed.map(_.y.toString),
          rotation = Some(entity.rotation.toString),
          angularSpeed = entity.angularSpeed.map(_.toString)
        )

  /**
   * Builds the complete ordered entity field specification.
   *
   * @param teams
   *   teams offered by the team selection
   * @param defaultValues
   *   initial values displayed by the fields
   * @return
   *   entity fields, including shape-dependent dimensions
   */
  private[forms] def buildFields(
      teams: Seq[Team],
      defaultValues: SaveEntityFormDefaultValues
  ): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(
        id = EntityFormParser.PositionXKey,
        label = "Initial X Position",
        defaultValue = defaultValues.x
      ),
      TextFieldSpec(
        id = EntityFormParser.PositionYKey,
        label = "Initial Y Position",
        defaultValue = defaultValues.y
      ),
      SelectFieldSpec(
        id = EntityFormParser.ShapeKey,
        label = "Shape",
        options = Shapes,
        dependentFields = FormDialog.buildShapeFields(
          radiusDefaultValue = defaultValues.radius,
          heightDefaultValue = defaultValues.height,
          widthDefaultValue = defaultValues.length
        ),
        defaultValue = defaultValues.shape
      ),
      TextFieldSpec(
        id = EntityFormParser.SpeedXKey,
        label = "Initial Speed X",
        defaultValue = defaultValues.speedX
      ),
      TextFieldSpec(
        id = EntityFormParser.SpeedYKey,
        label = "Initial Speed Y",
        defaultValue = defaultValues.speedY
      ),
      TextFieldSpec(
        id = EntityFormParser.RotationKey,
        label = "Initial Rotation (degrees)",
        defaultValue = defaultValues.rotation
      ),
      TextFieldSpec(
        id = EntityFormParser.AngularSpeedKey,
        label = "Angular Speed (degrees/s)",
        defaultValue = defaultValues.angularSpeed
      ),
      TextFieldSpec(
        id = EntityFormParser.WeightKey,
        label = "Weight",
        defaultValue = defaultValues.weight
      ),
      TextFieldSpec(
        id = EntityFormParser.HealthKey,
        label = "Health",
        defaultValue = defaultValues.health
      ),
      TextFieldSpec(
        id = EntityFormParser.DamageKey,
        label = "Damage",
        defaultValue = defaultValues.damage
      ),
      SelectFieldSpec(
        id = EntityFormParser.TeamIdKey,
        label = "Team",
        options = teams.map(_.id.value),
        defaultValue = defaultValues.teamId
      )
    )

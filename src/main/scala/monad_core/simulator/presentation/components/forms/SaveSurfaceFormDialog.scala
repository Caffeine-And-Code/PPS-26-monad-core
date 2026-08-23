package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.Surface
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.*
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.LocatableFormShapes.{
  getDefaultValuesByShape,
  getEnumValue
}
import monad_core.simulator.presentation.components.forms.parsers.{
  LocatableFormShapes,
  SurfaceFormParser
}
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

final case class SaveSurfaceFormDialogProps(
    title: String,
    onSubmit: Surface => Unit,
    onError: BaseError => Unit,
    anchorNode: Option[Node] = None,
    surfaceToUpdate: Option[Surface] = None
)

private object SurfaceFormDefaults:
  val InitialX: String     = "10.0"
  val InitialY: String     = "10.0"
  val InitialShape: String = LocatableFormShapes.CircleLabel

private case class SaveSurfaceFormDefaultValues(
    x: Option[String] = Option.apply(SurfaceFormDefaults.InitialX),
    y: Option[String] = Option.apply(SurfaceFormDefaults.InitialY),
    shape: Option[String] = Option.apply(SurfaceFormDefaults.InitialShape),
    rotation: Option[String] = Some("0.0"),
    frictionIndex: Option[String] = None,
    appliedForceX: Option[String] = None,
    appliedForceY: Option[String] = None,
    radius: Option[String] = None,
    height: Option[String] = None,
    length: Option[String] = None
)

object SaveSurfaceFormDialog:

  private[forms] val Shapes =
    Seq(LocatableFormShapes.CircleLabel, LocatableFormShapes.RectangleLabel)

  private case class SaveSurfaceViewModel(surfaceToUpdate: Option[Surface])

  extension (viewModel: SaveSurfaceViewModel)

    // Decide se costruire una nuova Surface o aggiornarne una esistente
    // (mantenendone l'id), in base ai valori raccolti dal form.
    private def resolveSurface(values: Map[String, String]): Either[BaseError, Surface] =
      viewModel.surfaceToUpdate match
        case Some(surface) => SurfaceFormParser.buildSurface(values, () => surface.id.value)
        case None          => SurfaceFormParser.buildSurface(values)

  def show(props: SaveSurfaceFormDialogProps): Either[BaseError, Unit] = {
    val defaultValues = buildDefaultValues(props.surfaceToUpdate)
    val viewModel     = SaveSurfaceViewModel(props.surfaceToUpdate)

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = buildFields(defaultValues),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit =
          values => viewModel.resolveSurface(values).matchToResult(props.onError)(props.onSubmit)
      )
    )
  }

  private[forms] def buildDefaultValues(
      surfaceToUpdate: Option[Surface]
  ): SaveSurfaceFormDefaultValues =
    surfaceToUpdate match
      case None => SaveSurfaceFormDefaultValues()
      case Some(surface) =>
        val (radius, height, length) = surface.shape.getDefaultValuesByShape

        SaveSurfaceFormDefaultValues(
          x = Some(surface.position.x.toString),
          y = Some(surface.position.y.toString),
          shape = Some(surface.shape.getEnumValue.getStringValue),
          radius = radius,
          height = height,
          length = length,
          rotation = Some(surface.rotation.toString),
          appliedForceX = surface.appliedForce.flatMap(vector => Some(vector.x.toString)),
          appliedForceY = surface.appliedForce.flatMap(vector => Some(vector.y.toString)),
          frictionIndex = surface.frictionIndex.map(_.toString)
        )

  private[forms] def buildFields(defaultValues: SaveSurfaceFormDefaultValues): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(
        id = SurfaceFormParser.PositionXKey,
        label = "Initial X Position",
        defaultValue = defaultValues.x
      ),
      TextFieldSpec(
        id = SurfaceFormParser.PositionYKey,
        label = "Initial Y Position",
        defaultValue = defaultValues.y
      ),
      SelectFieldSpec(
        id = SurfaceFormParser.ShapeKey,
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
        id = SurfaceFormParser.RotationKey,
        label = "Initial Rotation (degrees)",
        defaultValue = defaultValues.rotation
      ),
      TextFieldSpec(
        id = SurfaceFormParser.AppliedForceXKey,
        label = "Applied Force X",
        defaultValue = defaultValues.appliedForceX
      ),
      TextFieldSpec(
        id = SurfaceFormParser.AppliedForceYKey,
        label = "Applied Force Y",
        defaultValue = defaultValues.appliedForceY
      ),
      TextFieldSpec(
        id = SurfaceFormParser.FrictionIndexKey,
        label = "Friction Index",
        defaultValue = defaultValues.frictionIndex
      )
    )

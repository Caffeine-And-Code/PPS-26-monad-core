package monad_core.simulator.presentation.components.forms.base

import monad_core.simulator.CannotBuildDialog
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.*
import monad_core.simulator.presentation.components.forms.parsers.{
  BaseFormParser,
  LocatableFormShapes
}
import scalafx.stage.Window

import scala.util.Try

/**
 * Configuration of a declarative form dialog.
 *
 * @param title
 *   title displayed by the dialog stage
 * @param fields
 *   fields rendered in declaration order
 * @param onSubmit
 *   callback receiving current values indexed by field identifier
 * @param owner
 *   optional owner used for window modality
 * @param minWidth
 *   minimum width of the dialog content
 */
final case class FormDialogProps(
    title: String,
    fields: Seq[FormFieldSpec],
    onSubmit: Map[String, String] => Unit,
    owner: Option[Window] = None,
    minWidth: Double = 500,
    submitLabel: String = "Save"
)

/** Entry point for rendering and submitting declarative ScalaFX forms. */
object FormDialog:

  /** Classpath location of the shared form stylesheet. */
  private[forms] val StylesheetPath: String =
    getClass.getResource("/stylesheets/form-dialog.css").toExternalForm

  /**
   * Builds and displays a form dialog.
   *
   * @param props
   *   dialog configuration
   * @return
   *   `Right(())` after the stage is displayed, or `Left(CannotBuildDialog)` if construction fails
   */
  def show(props: FormDialogProps): Either[BaseError, Unit] =
    Try {
      val builder = new FormDialogBuilder(props)
      builder.display()
    }.toEither.left.map(ex => CannotBuildDialog(ex.getMessage, "FormDialog"))

  /**
   * Builds the dependent dimensions required by each supported shape.
   *
   * @param radiusDefaultValue
   *   initial radius for a circle
   * @param widthDefaultValue
   *   initial rectangle width
   * @param heightDefaultValue
   *   initial rectangle height
   * @return
   *   shape labels mapped to their corresponding dimension fields
   */
  def buildShapeFields(
      radiusDefaultValue: Option[String],
      widthDefaultValue: Option[String],
      heightDefaultValue: Option[String]
  ): Map[String, Seq[FormFieldSpec]] =
    Map(
      LocatableFormShapes.CircleLabel -> Seq(
        TextFieldSpec(
          id = BaseFormParser.RadiusKey,
          label = "Radius",
          defaultValue = radiusDefaultValue
        )
      ),
      LocatableFormShapes.RectangleLabel -> Seq(
        TextFieldSpec(
          id = BaseFormParser.LengthKey,
          label = "Width",
          defaultValue = widthDefaultValue
        ),
        TextFieldSpec(
          id = BaseFormParser.HeightKey,
          label = "Height",
          defaultValue = heightDefaultValue
        )
      )
    )

  extension [RightType](either: Either[BaseError, RightType])

    /**
     * Dispatches an `Either` to the appropriate form callback.
     *
     * @param onError
     *   callback invoked for a failed result
     * @param onRightResult
     *   callback invoked for a successful result
     */
    private[forms] def matchToResult(onError: BaseError => Unit)(
        onRightResult: RightType => Unit
    ): Unit =
      either match
        case Left(error)   => onError(error)
        case Right(result) => onRightResult(result)

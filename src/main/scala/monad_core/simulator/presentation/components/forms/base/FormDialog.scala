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

final case class FormDialogProps(
    title: String,
    fields: Seq[FormFieldSpec],
    onSubmit: Map[String, String] => Unit,
    owner: Option[Window] = None,
    minWidth: Double = 500,
    submitLabel: String = "Save"
)

object FormDialog:

  private[forms] val StylesheetPath: String =
    getClass.getResource("/stylesheets/form-dialog.css").toExternalForm

  def show(props: FormDialogProps): Either[BaseError, Unit] =
    Try {
      val builder = new FormDialogBuilder(props)
      builder.display()
    }.toEither.left.map(ex => CannotBuildDialog(ex.getMessage, "FormDialog"))

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

    private[forms] def matchToResult(onError: BaseError => Unit)(
        onRightResult: RightType => Unit
    ): Unit =
      either match
        case Left(error)   => onError(error)
        case Right(result) => onRightResult(result)

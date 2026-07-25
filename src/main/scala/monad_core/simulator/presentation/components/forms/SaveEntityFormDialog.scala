package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Entity, Team}
import monad_core.simulator.presentation.components.forms.parsers.{EntityFormParser, EntityShapes}
import scalafx.stage.Window

final case class SaveEntityFormDialogProps(
                                            title: String,
                                            onSubmit: Entity => Unit,
                                            onError: EngineError => Unit,
                                            teams: Seq[Team],
                                            owner: Option[Window] = None
                                          )

object SaveEntityFormDialog:
  private val Shapes = Seq(EntityShapes.CircleLabel, EntityShapes.RectangleLabel)

  def show(props: SaveEntityFormDialogProps): Either[EngineError, Unit] =
    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = buildFields(props.teams),
        owner = props.owner,
        onSubmit = values =>
          EntityFormParser.buildEntity(values) match
            case Right(entity) => props.onSubmit(entity)
            case Left(error) => props.onError(error)
      )
    )

  private def buildFields(teams: Seq[Team]): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(id = "x", label = "Initial X Position", defaultValue = Some("10.0")),
      TextFieldSpec(id = "y", label = "Initial Y Position", defaultValue = Some("10.0")),
      SelectFieldSpec(
        id = "shape",
        label = "Shape",
        options = Shapes,
        dependentFields = Map(
          EntityShapes.CircleLabel -> Seq(
            TextFieldSpec(id = "radius", label = "Radius")
          ),
          EntityShapes.RectangleLabel -> Seq(
            TextFieldSpec(id = "height", label = "Height"),
            TextFieldSpec(id = "length", label = "Width")
          )
        )
      ),
      TextFieldSpec(id = "speed", label = "Initial Speed"),
      TextFieldSpec(id = "weight", label = "Weight"),
      TextFieldSpec(id = "health", label = "Health"),
      SelectFieldSpec(
        id = "teamId",
        label = "Team",
        options = teams.map(_.id.value)
      )
    )
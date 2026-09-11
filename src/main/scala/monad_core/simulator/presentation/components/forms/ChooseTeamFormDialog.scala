package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.Team
import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.{
  FormDialog,
  FormDialogProps,
  FormFieldSpec,
  SelectFieldSpec
}
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

/**
 * Configuration of a dialog that asks the user to select a team.
 *
 * @param teams
 *   teams available for selection
 * @param onSubmit
 *   callback invoked with the selected team
 * @param onError
 *   callback invoked if the submitted identifier no longer resolves to a team
 * @param anchorNode
 *   optional node whose window owns the dialog
 */
final case class ChooseTeamFormDialogProps(
    teams: Seq[Team],
    onSubmit: Team => Unit,
    onError: BaseError => Unit,
    anchorNode: Option[Node] = None
)

/** Builds the team-selection form and resolves its submitted identifier. */
object ChooseTeamFormDialog:

  /** Submitted-value key of the selected team. */
  private[forms] val TeamKey: String = "chosenTeam"

  private case class ChooseTeamViewModel(teams: Seq[Team])

  extension (viewModel: ChooseTeamViewModel)

    private def resolveSelectedTeam(values: Map[String, String]): Option[Either[BaseError, Team]] =
      values.get(TeamKey).map { selectedTeamId =>
        viewModel.teams
          .find(_.id.value == selectedTeamId)
          .toRight(TeamNotFoundDuringSelection(selectedTeamId))
      }

  /**
   * Displays a team-selection dialog.
   *
   * @param props
   *   available teams and result callbacks
   * @return
   *   the result of building and displaying the underlying form dialog
   */
  def show(props: ChooseTeamFormDialogProps): Either[BaseError, Unit] =
    val viewModel = ChooseTeamViewModel(props.teams)

    FormDialog.show(
      FormDialogProps(
        title = "Please choose a Team",
        fields = buildSelect(props.teams),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit = values =>
          viewModel.resolveSelectedTeam(values).foreach {
            case Right(team) => props.onSubmit(team)
            case Left(error) => props.onError(error)
          }
      )
    )

  /**
   * Builds the single-choice field used by the dialog.
   *
   * @param teams
   *   teams whose identifiers become selectable options
   * @return
   *   the team selection field
   */
  private[forms] def buildSelect(teams: Seq[Team]): Seq[FormFieldSpec] =
    Seq(
      SelectFieldSpec(
        id = TeamKey,
        label = "Team",
        options = teams.map(_.id.value)
      )
    )

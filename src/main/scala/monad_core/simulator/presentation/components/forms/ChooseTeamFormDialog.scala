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

final case class ChooseTeamFormDialogProps(
                                            teams: Seq[Team],
                                            onSubmit: Team => Unit,
                                            onError: BaseError => Unit,
                                            anchorNode: Option[Node] = None
                                          )

object ChooseTeamFormDialog:

  private[forms] val TeamKey: String = "chosenTeam"

  private case class ChooseTeamViewModel(teams: Seq[Team])

  extension (viewModel: ChooseTeamViewModel)

    private def resolveSelectedTeam(values: Map[String, String]): Option[Either[BaseError, Team]] =
      values.get(TeamKey).map { selectedTeamId =>
        viewModel.teams
          .find(_.id.value == selectedTeamId)
          .toRight(TeamNotFoundDuringSelection(selectedTeamId))
      }

  def show(props: ChooseTeamFormDialogProps): Either[BaseError, Unit] =
    val viewModel = ChooseTeamViewModel(props.teams)

    FormDialog.show(
      FormDialogProps(
        title = "Please choose a Team",
        fields = buildSelect(props.teams),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit = values =>
          viewModel.resolveSelectedTeam(values).foreach {
            case Right(team)  => props.onSubmit(team)
            case Left(error)  => props.onError(error)
          }
      )
    )

  private[forms] def buildSelect(teams: Seq[Team]): Seq[FormFieldSpec] =
    Seq(
      SelectFieldSpec(
        id = TeamKey,
        label = "Team",
        options = teams.map(_.id.value)
      )
    )
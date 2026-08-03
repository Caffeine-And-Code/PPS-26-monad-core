package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.TeamNotFoundDuringSelection
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.{FormDialog, FormDialogProps, FormFieldSpec, SelectFieldSpec}
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

  def show(props: ChooseTeamFormDialogProps): Either[BaseError, Unit] = {
    FormDialog.show(
      FormDialogProps(
        title = "Please choose a Team",
        fields = buildSelect(props.teams),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit = values =>
          if values.contains(TeamKey) then {
            val selectedTeamId = values(TeamKey)
            props.teams.find(_.id.value == selectedTeamId) match
              case Some(team) => props.onSubmit(team)
              case None => props.onError(TeamNotFoundDuringSelection(selectedTeamId))
          }
      )
    )
  }

  private[forms] def buildSelect(teams: Seq[Team]): Seq[FormFieldSpec] =
    Seq(
      SelectFieldSpec(
        id = TeamKey,
        label = "Team",
        options = teams.map(_.id.value)
      )
    )

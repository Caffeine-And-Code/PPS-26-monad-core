package monad_core.simulator.presentation.components.forms

import monad_core.engine.errors.EngineError
import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.TeamNotFoundDuringSelection
import scalafx.stage.Window

final case class ChooseTeamFormDialogProps(
                                            teams: Seq[Team],
                                            onSubmit: Team => Unit,
                                            onError: EngineError => Unit,
                                            owner: Option[Window] = None
                                          )

object ChooseTeamFormDialog:
  private[forms] val TeamKey: String = "chosenTeam"

  def show(props: ChooseTeamFormDialogProps): Either[EngineError, Unit] = {
    FormDialog.show(
      FormDialogProps(
        title = "Please choose a Team",
        fields = buildFields(props.teams),
        owner = props.owner,
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

  private[forms] def buildFields(teams: Seq[Team]): Seq[FormFieldSpec] =
    Seq(
      SelectFieldSpec(
        id = TeamKey,
        label = "Team",
        options = teams.map(_.id.value)
      )
    )

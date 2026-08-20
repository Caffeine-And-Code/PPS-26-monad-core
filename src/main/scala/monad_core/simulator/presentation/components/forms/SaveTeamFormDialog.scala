package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.*
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.TeamFormParser
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

final case class SaveTeamFormDialogProps(
    title: String,
    onSubmit: Team => Unit,
    onError: BaseError => Unit,
    possibleEnemies: Seq[Team],
    anchorNode: Option[Node] = None,
    teamToUpdate: Option[Team] = None
)

final case class SaveTeamFormDefaultValues(
    teamName: Option[TeamId] = Option.empty,
    enemies: Seq[TeamId] = Seq.empty
)

final private[forms] case class BuildSaveTeamFormFieldsRecord(
    possibleEnemies: Seq[Team],
    defaultValues: SaveTeamFormDefaultValues
)

object SaveTeamFormDialog:

  private case class SaveTeamViewModel(teamToUpdate: Option[Team])

  extension (viewModel: SaveTeamViewModel)

    private def resolveTeam(values: Map[String, String]): Either[BaseError, Team] =
      viewModel.teamToUpdate match
        case None          => TeamFormParser.buildTeam(values)
        case Some(oldTeam) => TeamFormParser.buildUpdatedTeam(values, oldTeam)

    private def resolveFields(record: BuildSaveTeamFormFieldsRecord): Seq[FormFieldSpec] =
      viewModel.teamToUpdate match
        case Some(_) => buildTeamEditFields(record)
        case None    => buildTeamCreationFields(record)

  def show(props: SaveTeamFormDialogProps): Either[BaseError, Unit] = {
    val defaultValues     = buildDefaultValues(props.teamToUpdate)
    val buildFieldsRecord = BuildSaveTeamFormFieldsRecord(props.possibleEnemies, defaultValues)
    val viewModel         = SaveTeamViewModel(props.teamToUpdate)

    FormDialog.show(
      FormDialogProps(
        title = props.title,
        fields = viewModel.resolveFields(buildFieldsRecord),
        owner = ScalaFxUtils.ownerWindowOfOption(props.anchorNode),
        onSubmit =
          values => viewModel.resolveTeam(values).matchToResult(props.onError)(props.onSubmit)
      )
    )
  }

  private[forms] def buildDefaultValues(
      teamToUpdate: Option[Team]
  ): SaveTeamFormDefaultValues =
    teamToUpdate match
      case None => SaveTeamFormDefaultValues()
      case Some(team) =>
        SaveTeamFormDefaultValues(
          teamName = Some(team.id),
          enemies = Seq.from(team.enemies)
        )

  private[forms] def buildTeamCreationFields(
      record: BuildSaveTeamFormFieldsRecord
  ): Seq[FormFieldSpec] =
    Seq(
      TextFieldSpec(
        id = TeamFormParser.TeamIdKey,
        label = "Name",
        defaultValue = record.defaultValues.teamName.map(_.value)
      )
    ).appendedAll(
      buildTeamEditFields(record)
    )

  private[forms] def buildTeamEditFields(
      record: BuildSaveTeamFormFieldsRecord
  ): Seq[FormFieldSpec] =
    Seq(
      MultiSelectFieldSpec(
        id = TeamFormParser.EnemiesKey,
        label = "Enemies",
        options = record.possibleEnemies.map(_.id.value),
        defaultValues = record.defaultValues.enemies.map(_.value)
      )
    )

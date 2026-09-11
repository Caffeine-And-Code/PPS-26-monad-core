package monad_core.simulator.presentation.components.forms

import monad_core.engine.model.{Team, TeamId}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.forms.base.*
import monad_core.simulator.presentation.components.forms.base.FormDialog.matchToResult
import monad_core.simulator.presentation.components.forms.parsers.TeamFormParser
import monad_core.simulator.presentation.support.ScalaFxUtils
import scalafx.scene.Node

/**
 * Configuration of a team creation or editing dialog.
 *
 * @param title
 *   title displayed by the dialog
 * @param onSubmit
 *   callback invoked with the validated team
 * @param onError
 *   callback invoked when submitted values cannot produce a team
 * @param possibleEnemies
 *   teams offered by the enemy selection
 * @param anchorNode
 *   optional node whose window owns the dialog
 * @param teamToUpdate
 *   existing team to edit; `None` selects creation mode
 */
final case class SaveTeamFormDialogProps(
    title: String,
    onSubmit: Team => Unit,
    onError: BaseError => Unit,
    possibleEnemies: Seq[Team],
    anchorNode: Option[Node] = None,
    teamToUpdate: Option[Team] = None
)

/**
 * Initial values displayed by a team form.
 *
 * @param teamName
 *   initial identifier
 * @param enemies
 *   initially selected enemy identifiers
 */
final case class SaveTeamFormDefaultValues(
    teamName: Option[TeamId] = Option.empty,
    enemies: Seq[TeamId] = Seq.empty
)

/**
 * Input required to construct the fields of a team form.
 *
 * @param possibleEnemies
 *   teams offered by the enemy selection
 * @param defaultValues
 *   values initially displayed by the form
 */
final private[forms] case class BuildSaveTeamFormFieldsRecord(
    possibleEnemies: Seq[Team],
    defaultValues: SaveTeamFormDefaultValues
)

/** Builds team forms and converts their submitted values into engine teams. */
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

  /**
   * Displays a team creation or editing dialog.
   *
   * Editing mode preserves the existing team identifier and displays only the enemy selection. Parsing and domain
   * errors produced on submission are sent to `props.onError`.
   *
   * @param props
   *   dialog mode, possible enemies and result callbacks
   * @return
   *   the result of building and displaying the underlying form dialog
   */
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

  /**
   * Derives field defaults for creation or editing mode.
   *
   * @param teamToUpdate
   *   team whose identifier and enemies populate the defaults
   * @return
   *   creation defaults when absent, otherwise values extracted from the team
   */
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

  /**
   * Builds fields for team creation.
   *
   * @param record
   *   possible enemies and initial values
   * @return
   *   identifier field followed by the enemy selection
   */
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

  /**
   * Builds fields for team editing.
   *
   * @param record
   *   possible enemies and initial values
   * @return
   *   the enemy selection field; the existing identifier is not editable
   */
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

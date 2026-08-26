package monad_core.simulator.presentation.panels

import monad_core.engine.model.{Entity, Surface, Team, TeamId}
import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.*
import monad_core.simulator.presentation.components.forms.*
import monad_core.simulator.presentation.panels.support.FormUtilities.{
  displayError,
  onActionMakeSnapshot
}
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.resources.Image.*
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, Region, VBox}

/**
 *  GameEngineModePanel concrete builder
 */
object GameEngineModePanel extends GameEngineModePanelBuilder:

  /**
   * ViewModel used as state of the panel component
   * @param world [[World]]
   * @param gameEngineRuntime [[GameEngineRuntime]]
   */
  private case class GameEngineModeViewModel(world: World, gameEngineRuntime: GameEngineRuntime):
    val editTeamsDisabled: BooleanProperty   = BooleanProperty(true)
    val deleteTeamsDisabled: BooleanProperty = BooleanProperty(true)

  extension (viewModel: GameEngineModeViewModel)

    /**
     * The Sole point where [[viewModel.editTeamsDisabled]] and [[viewModel.deleteTeamsDisabled]]
     * state variables are changed based on the world teams count.
     *
     * - [[viewModel.editTeamsDisabled]] is set to `true` when world teams are more than one
     * - [[viewModel.deleteTeamsDisabled]] is set to `true` when world teams are more than zero
     */
    private def refreshTeamAvailability(): Unit =
      val teams = viewModel.world.getAllTeams

      viewModel.editTeamsDisabled.value = teams.length <= 1
      viewModel.deleteTeamsDisabled.value = teams.isEmpty

    /**
     * Wrapper that runs [[refreshTeamAvailability]] when result is `Right`
     * @param result the provided result
     * @return the propagated result from result itself
     */
    private def refreshTeamsAfter(
        result: Either[BaseError, Unit]
    ): Either[BaseError, Unit] =
      result.map { _ =>
        viewModel.refreshTeamAvailability()
      }

    /**
     * Wraps the [[World.createEntity]] function to handle and display a possible error returned by it.
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result, or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.createEntity]]
     * @param entity the entity that needs to be created
     */
    private def addEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveEntityCommand(entity))(viewModel.world.createEntity)

    /**
     * Wraps the [[World.createSurface]] function to handle and display a possible error returned by it.
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result, or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.createSurface]]
     * @param surface the surface that needs to be created
     */
    private def addSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveSurfaceCommand(surface))(viewModel.world.createSurface)

    /**
     * Wraps the [[World.createTeam]] function to handle and display a possible error returned by it.
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result, or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.createTeam]]
     * @param team the team that needs to be created
     */
    private def addTeam(team: Team): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveTeamCommand(team))(command =>
        viewModel.refreshTeamsAfter(viewModel.world.createTeam(command))
      )

    /**
     * Wraps the [[World.updateTeam]] function to handle and display a possible error returned by it.
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result, or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.updateTeam]]
     * @param team the team that needs to be updated
     */
    private def updateTeam(team: Team): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveTeamCommand(team))(command => viewModel.world.updateTeam(command))

    /**
     * Wraps the [[World.removeTeam]] function to handle and display a possible error returned by it.
     * By doing so [[onActionMakeSnapshot]] is used to make a snapshot on `Right` result, or display the error on `Left`.
     *
     * @see [[onActionMakeSnapshot]], [[World.removeTeam]]
     * @param teamId the id of the team that needs to be deleted
     */
    private def deleteTeam(teamId: TeamId): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(teamId)(id =>
        viewModel.refreshTeamsAfter(viewModel.world.removeTeam(id.value))
      )

    /**
     * Permits the component to enable/disable a physics rule by calling the [[GameEngineRuntime.setPhysicsRuleEnabled]] function.
     * @param ruleId the rule specific id
     * @param isEnabled `true` if the rule needs to be enabled, `false` otherwise
     */
    private def setPhysicsRuleEnabled(ruleId: String, isEnabled: Boolean): Unit =
      viewModel.gameEngineRuntime.setPhysicsRuleEnabled(ruleId, isEnabled)

  /**
   * Imperative shell that constructs the panel.
   *
   * It constructs each button and displays them in a row layout, separating the menu button and physic
   * rule button from the engine mode control buttons.
   *
   * @see [[IconButton]] and [[MenuButton]]
   * @param imageConfig system configuration to handle the images
   * @param onModeChange callback run each time the mode button is clicked
   * @param onResetClick callback run each time the reset button is clicked
   * @param isEngineRunning [[BooleanProperty]] representing if the engine is currently running or not
   * @param world [[World]]
   * @param gameEngineRuntime [[GameEngineRuntime]]
   * @return
   */
  def build(
      imageConfig: ImageConfigRecord,
      onModeChange: Boolean => Unit,
      onResetClick: () => Unit,
      isEngineRunning: BooleanProperty
  )(using
      world: World,
      gameEngineRuntime: GameEngineRuntime
  ): Either[BaseError, VBox] =

    val viewModel = GameEngineModeViewModel(world, gameEngineRuntime)

    for
      playPauseBtn <- IconButton
        .buildToggle(
          defaultImage = PlayIcon(),
          activeImage = PauseIcon(),
          props = IconButtonBaseProps(
            imageConfig,
            onClick = isActive =>
              isEngineRunning.value = isActive
              onModeChange(isActive)
          ),
          activeProperty = isEngineRunning
        )
        .left
        .map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      resetBtn <- IconButton
        .build(
          StopIcon(),
          IconButtonBaseProps(
            imageConfig,
            isDisabled = !isEngineRunning,
            onClick = isActive =>
              isEngineRunning.value = false
              onResetClick()
          )
        )
        .left
        .map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      contextMenuAnchor = Some(playPauseBtn)

      menuBtn <- MenuButton
        .build(
          MenuButtonProps(
            isDisabled = isEngineRunning,
            imageConfig = imageConfig,
            defaultImage = ToolsIcon(),
            items = Seq(
              MenuButtonItem(
                "Add an Entity",
                () =>
                  SaveEntityFormDialog.show(
                    props = SaveEntityFormDialogProps(
                      title = "Entity Settings",
                      anchorNode = contextMenuAnchor,
                      onSubmit = viewModel.addEntity,
                      teams = world.getAllTeams,
                      onError = displayError
                    )
                  )
              ),
              MenuButtonItem(
                isDisabled = isEngineRunning,
                label = "Add a Team",
                onSelect = () =>
                  SaveTeamFormDialog.show(
                    props = SaveTeamFormDialogProps(
                      title = "Team Settings",
                      anchorNode = contextMenuAnchor,
                      onSubmit = viewModel.addTeam,
                      possibleEnemies = world.getAllTeams,
                      onError = displayError
                    )
                  )
              ),
              MenuButtonItem(
                isDisabled = isEngineRunning,
                label = "Add a Surface",
                onSelect = () =>
                  SaveSurfaceFormDialog.show(
                    props = SaveSurfaceFormDialogProps(
                      title = "Surface Settings",
                      anchorNode = contextMenuAnchor,
                      onSubmit = viewModel.addSurface,
                      onError = displayError
                    )
                  )
              ),
              MenuButtonItem(
                label = "Edit a Team",
                onSelect = () =>
                  ChooseTeamFormDialog.show(
                    props = ChooseTeamFormDialogProps(
                      anchorNode = contextMenuAnchor,
                      onSubmit = team =>
                        SaveTeamFormDialog.show(
                          props = SaveTeamFormDialogProps(
                            title = s"${team.id} Settings",
                            anchorNode = contextMenuAnchor,
                            onSubmit = viewModel.updateTeam,
                            possibleEnemies =
                              world.getAllTeams.filterNot(_.id.value == team.id.value),
                            onError = displayError,
                            teamToUpdate = Some(team)
                          )
                        ),
                      teams = world.getAllTeams,
                      onError = displayError
                    )
                  ),
                isDisabled = viewModel.editTeamsDisabled || isEngineRunning
              ),
              MenuButtonItem(
                label = "Delete a Team",
                onSelect = () =>
                  ChooseTeamFormDialog.show(
                    props = ChooseTeamFormDialogProps(
                      anchorNode = contextMenuAnchor,
                      onSubmit = team => viewModel.deleteTeam(team.id),
                      teams = world.getAllTeams,
                      onError = displayError
                    )
                  ),
                isDisabled = viewModel.deleteTeamsDisabled || isEngineRunning
              )
            )
          )
        )
        .left
        .map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      physicsMenuBtn <- MenuButton
        .build(
          MenuButtonProps(
            imageConfig = imageConfig,
            defaultImage = PhysicsIcon(),
            items = gameEngineRuntime.physicsRules.map { rule =>
              CheckMenuButtonItem(
                label = rule.id,
                isSelected = rule.isEnabled,
                onToggle = isEnabled => viewModel.setPhysicsRuleEnabled(rule.id, isEnabled)
              )
            }
          )
        )
        .left
        .map(error => CannotBuildPanel(error, GameEngineModePanel.toString))
    yield
      val spacer = new Region()
      HBox.setHgrow(spacer, Priority.Always)

      new VBox {
        val buttonsRow: HBox = new HBox {
          spacing = 8
          alignment = Pos.CenterRight
          children = Seq(
            menuBtn,
            physicsMenuBtn,
            spacer,
            playPauseBtn,
            resetBtn
          )
        }

        VBox.setVgrow(buttonsRow, Priority.Always)

        children = Seq(buttonsRow)
        alignment = Pos.BottomRight
        style = PanelStyles.base
      }

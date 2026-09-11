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

/** Builds the controls for engine execution, world editing, team management, and physics rules. */
object GameEngineModePanel extends GameEngineModePanelBuilder:

  /**
   * Mutable UI state derived from the current world and engine runtime.
   *
   * @param world world edited by the panel
   * @param gameEngineRuntime runtime controlled by the panel
   */
  private case class GameEngineModeViewModel(world: World, gameEngineRuntime: GameEngineRuntime):
    val editTeamsDisabled: BooleanProperty   = BooleanProperty(true)
    val deleteTeamsDisabled: BooleanProperty = BooleanProperty(true)

  extension (viewModel: GameEngineModeViewModel)

    /**
     * Synchronizes team-action availability with the number of teams in the world.
     *
     * Editing is disabled when fewer than two teams exist, while deletion is disabled
     * when the world contains no teams.
     */
    private def refreshTeamAvailability(): Unit =
      val teams = viewModel.world.getAllTeams

      viewModel.editTeamsDisabled.value = teams.length <= 1
      viewModel.deleteTeamsDisabled.value = teams.isEmpty

    /**
     * Refreshes team-action availability after a successful operation and preserves failures.
     *
     * @param result result of the team operation
     * @return the original failure, or `Right(Unit)` after refreshing the UI state
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
     * Enables or disables a physics rule through the game-engine runtime.
     *
     * @param ruleId identifier of the physics rule
     * @param isEnabled `true` if the rule needs to be enabled, `false` otherwise
     */
    private def setPhysicsRuleEnabled(ruleId: String, isEnabled: Boolean): Unit =
      viewModel.gameEngineRuntime.setPhysicsRuleEnabled(ruleId, isEnabled)

  /**
   * Imperative shell that constructs the panel.
   *
   * It constructs the world-editing and physics-rule menus alongside the engine mode
   * and reset controls, then arranges them in a row layout.
   *
   * @see [[monad_core.simulator.presentation.components.IconButton IconButton]]
   * @see [[monad_core.simulator.presentation.components.MenuButton MenuButton]]
   * @param imageConfig image-loading configuration used by the controls
   * @param onModeChange callback invoked with the requested running state
   * @param onResetClick callback invoked when the reset button is clicked
   * @param isEngineRunning observable property synchronized with the engine running state
   * @param world world edited by the panel
   * @param gameEngineRuntime runtime controlled by the panel
   * @return `Left(BaseError)` if a child control cannot be built, or `Right(VBox)` with the completed panel
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

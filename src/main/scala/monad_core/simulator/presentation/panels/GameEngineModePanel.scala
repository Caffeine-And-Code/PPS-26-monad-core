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
import monad_core.simulator.presentation.resources.Image.{PauseIcon, PlayIcon, StopIcon, ToolsIcon}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, Region, VBox}

object GameEngineModePanel extends GameEngineModePanelBuilder:

  private case class GameEngineModeViewModel(world: World, gameEngineRuntime: GameEngineRuntime):
    val editTeamsDisabled: BooleanProperty   = BooleanProperty(true)
    val deleteTeamsDisabled: BooleanProperty = BooleanProperty(true)

  extension (viewModel: GameEngineModeViewModel)

    private def refreshTeamAvailability(): Unit =
      val teams = viewModel.world.getAllTeams
      viewModel.editTeamsDisabled.value = teams.length <= 1
      viewModel.deleteTeamsDisabled.value = teams.isEmpty

    private def onTeamMutationResult(result: Either[BaseError, Unit]): Unit =
      result match
        case Left(error) => displayError(error)
        case Right(_)    => viewModel.refreshTeamAvailability()

    private def addEntity(entity: Entity): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveEntityCommand(entity), viewModel.world.createEntity)

    private def addSurface(surface: Surface): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveSurfaceCommand(surface), viewModel.world.createSurface)

    private def addTeam(team: Team): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(
        SaveTeamCommand(team),
        command => viewModel.onTeamMutationResult(viewModel.world.createTeam(command))
      )

    private def updateTeam(team: Team): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(SaveTeamCommand(team), command => viewModel.world.updateTeam(command))

    private def deleteTeam(teamId: TeamId): Unit =
      given GameEngineRuntime = viewModel.gameEngineRuntime

      onActionMakeSnapshot(
        teamId,
        id => viewModel.onTeamMutationResult(viewModel.world.removeTeam(id.value))
      )

  def build(
      imageConfig: ImageConfigRecord,
      onModeChange: Boolean => Unit,
      onStopClick: () => Unit,
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

      stopBtn <- IconButton
        .build(
          StopIcon(),
          IconButtonBaseProps(
            imageConfig,
            isDisabled = !isEngineRunning,
            onClick = isActive =>
              isEngineRunning.value = false
              onStopClick()
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
    yield
      val spacer = new Region()
      HBox.setHgrow(spacer, Priority.Always)

      new VBox {
        val buttonsRow: HBox = new HBox {
          spacing = 8
          alignment = Pos.CenterRight
          children = Seq(
            menuBtn,
            spacer,
            playPauseBtn,
            stopBtn
          )
        }

        VBox.setVgrow(buttonsRow, Priority.Always)

        children = Seq(buttonsRow)
        alignment = Pos.BottomRight
        style = PanelStyles.base
      }

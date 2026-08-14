package monad_core.simulator.presentation.panels

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
  getTeamsSafely,
  onActionMakeSnapshot
}
import monad_core.simulator.presentation.panels.support.PanelStyles
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.resources.Image.{PauseIcon, PlayIcon, StopIcon, ToolsIcon}
import monad_core.simulator.presentation.resources.ImageConfigRecord
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.layout.{HBox, Priority, Region, VBox}

object GameEngineModePanel extends GameEngineModePanelBuilder {

  def build(
      imageConfig: ImageConfigRecord,
      onModeChange: Boolean => Unit,
      onStopClick: () => Unit,
      isEngineRunning: BooleanProperty
  )(using
      world: World,
      gameEngineRuntime: GameEngineRuntime
  ): Either[BaseError, VBox] =

    val editTeamsIsDisabled   = BooleanProperty(true)
    val deleteTeamsIsDisabled = BooleanProperty(true)

    def onTeamAction(actionResult: Either[BaseError, Unit]): Unit =
      actionResult match
        case Left(error) => displayError(error)
        case Right(_) =>
          val teams = getTeamsSafely(world)

          editTeamsIsDisabled.value = teams.length <= 1
          deleteTeamsIsDisabled.value = teams.isEmpty

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
                      onSubmit = entity =>
                        onActionMakeSnapshot(SaveEntityCommand(entity), world.createEntity),
                      teams = getTeamsSafely(world),
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
                      onSubmit = team =>
                        onActionMakeSnapshot(
                          SaveTeamCommand(team),
                          command => onTeamAction(world.createTeam(command))
                        ),
                      possibleEnemies = getTeamsSafely(world),
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
                      onSubmit = surface =>
                        onActionMakeSnapshot(SaveSurfaceCommand(surface), world.createSurface),
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
                            onSubmit = team =>
                              onActionMakeSnapshot(
                                SaveTeamCommand(team),
                                command => world.updateTeam(command)
                              ),
                            possibleEnemies = getTeamsSafely(world).filterNot(_.id == team.id),
                            onError = displayError,
                            teamToUpdate = Some(team)
                          )
                        ),
                      teams = getTeamsSafely(world),
                      onError = displayError
                    )
                  ),
                isDisabled = editTeamsIsDisabled || isEngineRunning
              ),
              MenuButtonItem(
                label = "Delete a Team",
                onSelect = () =>
                  ChooseTeamFormDialog.show(
                    props = ChooseTeamFormDialogProps(
                      anchorNode = contextMenuAnchor,
                      onSubmit = team =>
                        onActionMakeSnapshot(team.id, id => onTeamAction(world.removeTeam(id))),
                      teams = getTeamsSafely(world),
                      onError = displayError
                    )
                  ),
                isDisabled = deleteTeamsIsDisabled || isEngineRunning
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

}

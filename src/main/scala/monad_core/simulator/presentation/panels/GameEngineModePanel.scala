package monad_core.simulator.presentation.panels

import monad_core.simulator.CannotBuildPanel
import monad_core.simulator.application.engine.GameEngineRuntime
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.components.*
import monad_core.simulator.presentation.components.forms.*
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
           )
           (
             using world: World,
             gameEngineRuntime: GameEngineRuntime
           ): Either[BaseError, VBox] = {
    val onFormError: BaseError => Unit = err => NotificationManager.show(err.message, Error)

    def onFormSubmit[T](submitResult: T, action: T => Unit): Unit =
      action(submitResult)
      gameEngineRuntime.createSnapshot()

    val editTeamsIsDisabled = BooleanProperty(true)
    val deleteTeamsIsDisabled = BooleanProperty(true)

    def onTeamAction(actionResult: Either[BaseError, Unit]): Unit =
      actionResult match
        case Left(error) => onFormError(error)
        case Right(_) =>
          editTeamsIsDisabled.value = world.getAllTeams.length <= 1
          deleteTeamsIsDisabled.value = world.getAllTeams.isEmpty

    for
      playPauseBtn <- IconButton.buildToggle(
          defaultImage = PlayIcon(),
          activeImage = PauseIcon(),
          props = IconButtonBaseProps(
            imageConfig,
            onClick =
              isActive =>
                isEngineRunning.value = isActive
                onModeChange(isActive)
          ),
          activeProperty = isEngineRunning
        )
        .left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      stopBtn <- IconButton.build(
          StopIcon(),
          IconButtonBaseProps(
            imageConfig,
            isDisabled = !isEngineRunning,
            onClick = isActive =>
              isEngineRunning.value = false
              onStopClick()
          )
        )
        .left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))

      contextMenuAnchor = Some(playPauseBtn)

      menuBtn <- MenuButton.build(
        MenuButtonProps(
          isDisabled = isEngineRunning,
          imageConfig = imageConfig,
          defaultImage = ToolsIcon(),
          items = Seq(
            MenuButtonItem("Add an Entity", () => SaveEntityFormDialog.show(
              props = SaveEntityFormDialogProps(
                title = "Entity Settings",
                anchorNode = contextMenuAnchor,
                onSubmit = entity => onFormSubmit(SaveEntityCommand(entity), world.createEntity),
                teams = world.getAllTeams,
                onError = onFormError
              )
            )),
            MenuButtonItem(
              isDisabled = isEngineRunning,
              label = "Add a Team",
              onSelect = () => SaveTeamFormDialog.show(
                props = SaveTeamFormDialogProps(
                  title = "Team Settings",
                  anchorNode = contextMenuAnchor,
                  onSubmit = team => onFormSubmit(SaveTeamCommand(team), command => onTeamAction(world.createTeam(command))),
                  possibleEnemies = world.getAllTeams,
                  onError = onFormError
                )
              )),
            MenuButtonItem(
              isDisabled = isEngineRunning,
              label = "Add a Surface",
              onSelect = () => SaveSurfaceFormDialog.show(
                props = SaveSurfaceFormDialogProps(
                  title = "Surface Settings",
                  anchorNode = contextMenuAnchor,
                  onSubmit = surface => onFormSubmit(SaveSurfaceCommand(surface), world.createSurface),
                  onError = onFormError
                )
              )),
            MenuButtonItem(
              label = "Edit a Team",
              onSelect = () => ChooseTeamFormDialog.show(
                props = ChooseTeamFormDialogProps(
                  anchorNode = contextMenuAnchor,
                  onSubmit = team =>
                    SaveTeamFormDialog.show(
                      props = SaveTeamFormDialogProps(
                        title = s"${team.id} Settings",
                        anchorNode = contextMenuAnchor,
                        onSubmit = team => onFormSubmit(SaveTeamCommand(team), command => world.updateTeam(command)),
                        possibleEnemies = world.getAllTeams.filterNot(_.id == team.id),
                        onError = onFormError,
                        teamToUpdate = Some(team)
                      )
                    ),
                  teams = world.getAllTeams,
                  onError = onFormError
                )
              ),
              isDisabled = editTeamsIsDisabled || isEngineRunning,
            ),
            MenuButtonItem(
              label = "Delete a Team",
              onSelect = () => ChooseTeamFormDialog.show(
                props = ChooseTeamFormDialogProps(
                  anchorNode = contextMenuAnchor,
                  onSubmit = team => onFormSubmit(team.id, id => onTeamAction(world.removeTeam(id))),
                  teams = world.getAllTeams,
                  onError = onFormError
                )
              ),
              isDisabled = deleteTeamsIsDisabled || isEngineRunning,
            ),
          )
        )
      ).left.map(error => CannotBuildPanel(error, GameEngineModePanel.toString))
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
}

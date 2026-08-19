package monad_core.simulator.presentation.components.ai

import monad_core.simulator.presentation.panels.support.BaseLabelStyle
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, Label}
import scalafx.scene.layout.{HBox, Priority, VBox}

object AiPanelHeader:

  final case class Props(
      onClear: () => Unit,
      modelName: String
  )

  final case class Model(clearDisabled: Boolean)

  def apply(props: Props): Component[Model, HBox] =
    val clearButton = new Button("CLEAR"):
      id = "chat-clear"
      prefWidth = 85
      style = clearButtonStyle
      onAction = _ => props.onClear()

    val titleLabel = new Label(s"Chat with Jimmy"):
      id = "chat-title"
      maxWidth = Double.MaxValue
      alignment = Pos.Center
      style = BaseLabelStyle.h1
      HBox.setHgrow(this, Priority.Always)

    val modelLabel = new Label(s"(${props.modelName})"):
      id = "chat-subtitle"
      alignment = Pos.Center
      style = BaseLabelStyle.silent
      HBox.setHgrow(this, Priority.Always)

    val titleSection = new VBox:
      spacing = 10
      children = Seq(
        titleLabel,
        modelLabel
      )

    val view = new HBox:
      spacing = 15
      alignment = Pos.Center
      children = Seq(titleSection, clearButton)

    Component(
      view,
      model => clearButton.disable = model.clearDisabled
    )

  private val clearButtonStyle =
    """
      |-fx-border-color: #5b6069;
      |-fx-border-width: 1;
      |-fx-border-radius: 10;
      |-fx-background-radius: 10;
      |-fx-background-color: #454951;
      |-fx-text-fill: white;
      |-fx-font-weight: bold;
      |-fx-cursor: hand;
      |""".stripMargin

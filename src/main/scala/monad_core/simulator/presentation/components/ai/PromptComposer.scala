package monad_core.simulator.presentation.components.ai

import scalafx.geometry.Pos
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{HBox, Priority}

object PromptComposer:
  final case class Props(
      onPromptChanged: String => Unit,
      onSubmit: () => Unit
  )

  final case class Model(
      prompt: String,
      inputDisabled: Boolean,
      submitDisabled: Boolean
  )

  def apply(props: Props): Component[Model, HBox] =
    val field = promptField(props.onSubmit)
    val sendButton = submitButton(props.onSubmit)

    field.text.onChange((_, _, prompt) => props.onPromptChanged(prompt))

    val view = new HBox:
      spacing = 15
      minHeight = 60
      alignment = Pos.Center
      children = Seq(field, sendButton)

    Component(
      view,
      model =>
        Option.when(field.text.value != model.prompt)(model.prompt).foreach {
          field.text = _
        }
        field.disable = model.inputDisabled
        sendButton.disable = model.submitDisabled
    )

  private def promptField(onSubmit: () => Unit): TextField =
    new TextField:
      id = "chat-prompt"
      promptText = "Ask to Jimmy"
      prefHeight = 60
      style = fieldStyle
      onAction = _ => onSubmit()
      HBox.setHgrow(this, Priority.Always)

  private def submitButton(onSubmit: () => Unit): Button =
    new Button("SEND"):
      id = "chat-send"
      prefWidth = 85
      prefHeight = 60
      style = sendButtonStyle
      onAction = _ => onSubmit()

  private val fieldStyle =
    """
      |-fx-border-color: #5b6069;
      |-fx-border-width: 1;
      |-fx-border-radius: 15;
      |-fx-background-radius: 15;
      |-fx-background-color: #34373c;
      |-fx-text-fill: white;
      |-fx-prompt-text-fill: #a9adb5;
      |-fx-padding: 0 20 0 20;
      |""".stripMargin

  private val sendButtonStyle =
    """
      |-fx-border-color: #5b6069;
      |-fx-border-width: 1;
      |-fx-border-radius: 15;
      |-fx-background-radius: 15;
      |-fx-background-color: #454951;
      |-fx-text-fill: white;
      |-fx-font-weight: bold;
      |-fx-cursor: hand;
      |""".stripMargin

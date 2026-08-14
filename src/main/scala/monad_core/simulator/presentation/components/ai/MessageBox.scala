package monad_core.simulator.presentation.components.ai

import monad_core.simulator.presentation.chat.{ChatMessage, MessageAuthor}
import monad_core.simulator.presentation.panels.support.BaseLabelStyle
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Label, ScrollPane}
import scalafx.scene.layout.{HBox, Priority, VBox}

object MessageBox:
  type Model = Seq[ChatMessage]

  def apply(): Component[Model, ScrollPane] =
    val messageList = new VBox:
      id = "chat-messages"
      spacing = 20
      padding = Insets(0, 10, 20, 0)

    val view = new ScrollPane:
      id = "chat-scroll"
      content = messageList
      fitToWidth = true
      pannable = true
      padding = Insets(10)
      style = "-fx-background: transparent; -fx-background-color: transparent;"
      VBox.setVgrow(this, Priority.Always)

    Component(
      view,
      messages =>
        messageList.children = messages.map(message)
        Platform.runLater(view.vvalue = 1.0)
    )

  private def message(chatMessage: ChatMessage): HBox =
    val isUserMessage = chatMessage.author == MessageAuthor.User

    new HBox:
      alignment = if isUserMessage then Pos.CenterRight else Pos.CenterLeft
      styleClass +=
        (if isUserMessage then "user-message" else "assistant-message")
      children = bubble(chatMessage.content, isUserMessage)

  private def bubble(content: String, isUserMessage: Boolean): VBox =
    new VBox:
      alignment = Pos.CenterLeft
      padding = Insets(20)
      prefWidth = 280
      maxWidth = 280
      style = bubbleStyle(if isUserMessage then "#454951" else "#34373c")
      children = new Label(content):
        wrapText = true
        style = BaseLabelStyle.p

  private def bubbleStyle(backgroundColor: String): String =
    s"""
       |-fx-border-color: #5b6069;
       |-fx-border-width: 1;
       |-fx-border-radius: 15;
       |-fx-background-radius: 15;
       |-fx-background-color: $backgroundColor;
       |""".stripMargin

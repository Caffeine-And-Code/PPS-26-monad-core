package monad_core.simulator.presentation.panels

import monad_core.engine.errors.EngineError
import monad_core.simulator.application.AgentService
import monad_core.simulator.presentation.chat.*
import monad_core.simulator.presentation.panels.support.{BaseLabelStyle, BasePanelStyle}
import monad_core.simulator.presentation.panels.traits.AiModelChatPanelBuilder
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{Button, Label, ScrollPane, TextField}
import scalafx.scene.layout.{HBox, Priority, VBox}

import scala.concurrent.ExecutionContext

object AiModelChatPanel extends AiModelChatPanelBuilder:

  override def build()(using
      agentService: AgentService,
      executionContext: ExecutionContext
  ): Either[EngineError, VBox] =
    val messages = new VBox {
      id = "chat-messages"
      spacing = 20
      padding = Insets(0, 10, 20, 0)
    }

    val messagesScrollPane = new ScrollPane {
      id = "chat-scroll"
      content = messages
      fitToWidth = true
      pannable = true
      padding = Insets(10)
      style = "-fx-background: transparent; -fx-background-color: transparent;"
    }
    VBox.setVgrow(messagesScrollPane, Priority.Always)

    def chatMessage(message: ChatMessage): HBox =
      val isUserMessage = message.author == MessageAuthor.User
      val messageAlignment = if isUserMessage then Pos.CenterRight else Pos.CenterLeft
      val backgroundColor = if isUserMessage then "#454951" else "#34373c"
      val roleStyleClass = if isUserMessage then "user-message" else "assistant-message"

      new HBox {
        alignment = messageAlignment
        styleClass += roleStyleClass
        children = new VBox {
          alignment = Pos.CenterLeft
          padding = Insets(20)
          prefWidth = 280
          maxWidth = 280
          style =
            s"""
               |-fx-border-color: #5b6069;
               |-fx-border-width: 1;
               |-fx-border-radius: 15;
               |-fx-background-radius: 15;
               |-fx-background-color: $backgroundColor;
            """.stripMargin
          children = new Label(message.content) {
            wrapText = true
            style = BaseLabelStyle.p
          }
        }
      }

    val promptField = new TextField {
      id = "chat-prompt"
      promptText = "Ask to Jimmy"
      HBox.setHgrow(this, Priority.Always)
      prefHeight = 60
      style =
        """
          |-fx-border-color: #5b6069;
          |-fx-border-width: 1;
          |-fx-border-radius: 15;
          |-fx-background-radius: 15;
          |-fx-background-color: #34373c;
          |-fx-text-fill: white;
          |-fx-prompt-text-fill: #a9adb5;
          |-fx-padding: 0 20 0 20;
        """.stripMargin
    }

    val sendButton = new Button("SEND") {
      id = "chat-send"
      prefWidth = 85
      prefHeight = 60
      style =
        """
          |-fx-border-color: #5b6069;
          |-fx-border-width: 1;
          |-fx-border-radius: 15;
          |-fx-background-radius: 15;
          |-fx-background-color: #454951;
          |-fx-text-fill: white;
          |-fx-font-weight: bold;
          |-fx-cursor: hand;
        """.stripMargin
    }

    val component = new VBox {
      spacing = 20
      padding = Insets(30)
      prefWidth = 500
      prefHeight = 550
      children = Seq(
        new Label("Chat with Jimmy") {
          id = "chat-title"
          maxWidth = Double.MaxValue
          alignment = Pos.Center
          style = BaseLabelStyle.h1
        },
        messagesScrollPane,
        new HBox {
          spacing = 15
          minHeight = 60
          alignment = Pos.Center
          children = Seq(promptField, sendButton)
        }
      )
      style = BasePanelStyle.get() + "-fx-padding: 30px;"
    }

    def scrollToLatestMessage(): Unit =
      Platform.runLater(messagesScrollPane.vvalue = 1.0)

    def visibleMessages(model: ChatPanelState): Seq[ChatMessage] =
      model match
        case ChatPanelState.Waiting(_) =>
          model.messages :+ ChatMessage("Loading ...", MessageAuthor.Assistant)
        case ChatPanelState.Error(_, _, error) =>
          model.messages :+ ChatMessage(s"Error: $error", MessageAuthor.Assistant)
        case ChatPanelState.Ready(_, _) => model.messages

    def render(model: ChatPanelState): Unit =
      val messagesToRender = visibleMessages(model)

      if promptField.text.value != model.prompt then
        promptField.text = model.prompt

      promptField.disable = model.isWaiting
      sendButton.disable = !model.canSend

      messages.children.clear()
      messagesToRender.foreach(message => messages.children.add(chatMessage(message)))
      scrollToLatestMessage()

    val viewModel = new ChatPanelViewModel(
      agentService,
      action => Platform.runLater(action())
    )

    viewModel.state.onChange((_, _, newState) => render(newState))

    promptField.text.onChange((_, _, newPrompt) =>
      viewModel.onPromptChange(newPrompt)
    )
    sendButton.onAction = _ => viewModel.onSubmit()
    promptField.onAction = _ => viewModel.onSubmit()
    render(viewModel.state.value)

    Right(component)

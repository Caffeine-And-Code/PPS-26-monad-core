package monad_core.simulator.presentation.panels

import monad_core.simulator.application.ai.AiAgent
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.chat.*
import monad_core.simulator.presentation.components.ai.{AiPanelHeader, MessageBox, PromptComposer}
import monad_core.simulator.presentation.panels.support.BaseLabelStyle
import monad_core.simulator.presentation.panels.traits.AiModelChatPanelBuilder
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.layout.VBox

import scala.concurrent.ExecutionContext

/** Builds the ScalaFX panel used to interact with the AI agent. */
object AiModelChatPanel extends AiModelChatPanelBuilder:

  final private case class ViewState(
      header: AiPanelHeader.Model,
      messages: MessageBox.Model,
      prompt: PromptComposer.Model
  )

  /**
   * Ai chat panel model UI component.
   *
   * @param aiAgent agent used by the panel
   * @param executionContext execution context for asynchronous agent calls
   * @return the initialized panel, or error if something fails
   */
  override def build(aiAgent: AiAgent)(using
      executionContext: ExecutionContext
  ): Either[BaseError, VBox] =
    val viewModel = new ChatPanelViewModel(
      aiAgent,
      action => Platform.runLater(action())
    )
    val header = AiPanelHeader(
      AiPanelHeader.Props(
        onClear = () => viewModel.onClearHistory(),
        modelName = aiAgent.getAgentInfo.modelName
      )
    )
    val messages = MessageBox()
    val prompt = PromptComposer(
      PromptComposer.Props(
        onPromptChanged = viewModel.onPromptChange,
        onSubmit = () => viewModel.onSubmit()
      )
    )

    val component = new VBox:
      spacing = 20
      padding = Insets(30)
      prefWidth = 500
      prefHeight = 550
      children = Seq(header.view, messages.view, prompt.view)
      style = BaseLabelStyle.p + "-fx-padding: 30px;"

    val render: ChatPanelState => Unit =
      toViewState.andThen { state =>
        header.render(state.header)
        messages.render(state.messages)
        prompt.render(state.prompt)
      }

    viewModel.state.onChange((_, _, newState) => render(newState))
    render(viewModel.state.value)

    Right(component)

  private val toViewState: ChatPanelState => ViewState = state =>
    ViewState(
      header = AiPanelHeader.Model(
        clearDisabled = state.messages.isEmpty || state.isWaiting
      ),
      messages = visibleMessages(state),
      prompt = PromptComposer.Model(
        prompt = state.prompt,
        inputDisabled = state.isWaiting,
        submitDisabled = !state.canSend
      )
    )

  private def visibleMessages: ChatPanelState => Seq[ChatMessage] =
    case ChatPanelState.Waiting(messages) =>
      messages :+ ChatMessage("Loading ...", MessageAuthor.Assistant)
    case ChatPanelState.Error(messages, _, error) =>
      messages :+ ChatMessage(s"Error: $error", MessageAuthor.Assistant)
    case ChatPanelState.Ready(messages, _) => messages

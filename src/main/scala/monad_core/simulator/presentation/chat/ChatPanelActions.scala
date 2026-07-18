package monad_core.simulator.presentation.chat

import monad_core.simulator.application.AgentService
import monad_core.simulator.presentation.chat.ChatPanelState.{Error, Ready, Waiting}
import monad_core.simulator.presentation.chat.MessageAuthor
import monad_core.simulator.presentation.chat.MessageAuthor.Assistant

case class ChatPanelActions(
                             state: ChatPanelState,
                             renderer: ChatPanelState => Unit
)

object ChatPanelActions:

  extension (actions: ChatPanelActions)

    def onPromptChange(newPrompt: String): ChatPanelActions =
      val newAction = actions.copy(state = actions.state.setPrompt(newPrompt))
      actions.renderer(newAction.state)
      newAction

    def onSubmit()(using agentService: AgentService): ChatPanelActions =
      val prompt = actions.state.prompt
      val currentState = actions.state.addMessage(ChatMessage(prompt, MessageAuthor.User))
      actions.renderer(currentState.toWaiting)
      val response = agentService.ask(prompt)
      val state = response match {
        case Right(response) => currentState.toReady.addMessage(ChatMessage(response, Assistant))
        case Left(error) => currentState.toError(error.message)
      }
      actions.renderer(state)
      actions.copy(state = state)

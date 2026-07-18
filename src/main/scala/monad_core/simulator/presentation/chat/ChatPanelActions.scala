package monad_core.simulator.presentation.chat

import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.chat.MessageAuthor.{Assistant, User}

object ChatPanelActions:

  def onPromptChange(state: ChatPanelState, newPrompt: String): ChatPanelState =
    state.setPrompt(newPrompt)

  def onSubmit(state: ChatPanelState): ChatPanelState =
    Option
      .when(state.canSend) {
        val prompt = state.prompt.trim
        state.addMessage(ChatMessage(prompt, User)).toWaiting
      }
      .getOrElse(state)

  def onAgentRespond(
      state: ChatPanelState,
      response: Either[BaseError, String]
  ): ChatPanelState =
    state match
      case waiting: ChatPanelState.Waiting =>
        response match
          case Right(answer) =>
            waiting.toReady.addMessage(ChatMessage(answer, Assistant))
          case Left(error) =>
            waiting.toError(error.message)
      case _ => state

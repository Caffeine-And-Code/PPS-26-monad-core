package monad_core.simulator.presentation.chat

import monad_core.simulator.domain.ai.AgentResponse
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
      response: Either[BaseError, AgentResponse]
  ): ChatPanelState =
    state match
      case waiting: ChatPanelState.Waiting =>
        response match
          case Right(answer) =>
            waiting.toReady.addMessage(ChatMessage(answer.response, Assistant))
          case Left(error) =>
            println(error.message)
            waiting.toError(error.message)
      case _ => state

  def onHistoryCleaned(
      state: ChatPanelState,
      result: Either[BaseError, Unit]
  ): ChatPanelState =
    result match
      case Right(_)    => ChatPanelState.Ready(Vector.empty, state.prompt)
      case Left(error) => state.toError(error.message)

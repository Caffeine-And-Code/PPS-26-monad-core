package monad_core.simulator.presentation.chat

import monad_core.simulator.domain.ai.AgentResponse
import monad_core.simulator.errors.BaseError
import monad_core.simulator.presentation.chat.MessageAuthor.{Assistant, User}

/** Chat panel actions. */
object ChatPanelActions:

  /**
   * On a prompt change action.
   *
   * @param state current state
   * @param newPrompt edited text
   * @return updated state
   * */
  def onPromptChange(state: ChatPanelState, newPrompt: String): ChatPanelState =
    state.setPrompt(newPrompt)

  /**
   * On prompt submit action.
   *
   * @param state current state
   * @return waiting state with the user message, or the unchanged state when submission is disabled
   */
  def onSubmit(state: ChatPanelState): ChatPanelState =
    Option
      .when(state.canSend) {
        val prompt = state.prompt.trim
        state.addMessage(ChatMessage(prompt, User)).toWaiting
      }
      .getOrElse(state)

  /**
   * Applies an agent response only while the panel is waiting.
   *
   * @param state current state
   * @param response agent response or error
   * @return ready or error state, or the unchanged state when no response is pending
   */
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
            waiting.toError(error.message)
      case _ => state

  /**
   * Clears the message list and set panel to ready.
   *
   * @param state current state
   * @param result history cleanup result
   * @return cleared ready state or an error state
   * */
  def onHistoryCleaned(
      state: ChatPanelState,
      result: Either[BaseError, Unit]
  ): ChatPanelState =
    result match
      case Right(_)    => ChatPanelState.Ready(Vector.empty, state.prompt)
      case Left(error) => state.toError(error.message)

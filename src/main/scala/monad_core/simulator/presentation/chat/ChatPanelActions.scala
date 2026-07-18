package monad_core.simulator.presentation.chat

import monad_core.simulator.presentation.chat.ChatPanelState.{Error, Ready, Waiting}

case class ChatPanelActions(state: ChatPanelState, renderer: ChatPanelState => Unit)

object ChatPanelActions:

  extension (actions: ChatPanelActions)

    def onPromptChange(newPrompt: String): ChatPanelActions = {
      val newAction = actions.copy(updateStateOnNewPrompt(newPrompt))
      actions.renderer(newAction.state)
      newAction
    }

    private def updateStateOnNewPrompt(newPrompt: String): ChatPanelState =
      actions.state match {
        case Ready(messages, _) => Ready(messages, newPrompt)
        case Error(messages, _, error) => Error(messages, newPrompt, error)
        case Waiting(messages) => Waiting(messages)
      }


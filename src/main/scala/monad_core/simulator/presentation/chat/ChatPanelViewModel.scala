package monad_core.simulator.presentation.chat

import monad_core.simulator.application.AgentService
import scalafx.beans.property.ObjectProperty

import scala.concurrent.ExecutionContext

final class ChatPanelViewModel(
    agentService: AgentService,
    runOnUiThread: (() => Unit) => Unit
)(using executionContext: ExecutionContext):

  val state: ObjectProperty[ChatPanelState] =
    ObjectProperty(ChatPanelState.initial)

  def onPromptChange(newPrompt: String): Unit =
    update(ChatPanelActions.onPromptChange(_, newPrompt))

  def onSubmit(): Unit =
    Option.when(state.value.canSend)(state.value.prompt.trim).foreach { prompt =>
      update(ChatPanelActions.onSubmit)

      agentService.ask(prompt).foreach { response =>
        runOnUiThread { () =>
          update(ChatPanelActions.onAgentRespond(_, response))
        }
      }
    }

  private def update(action: ChatPanelState => ChatPanelState): Unit =
    state.value = action(state.value)

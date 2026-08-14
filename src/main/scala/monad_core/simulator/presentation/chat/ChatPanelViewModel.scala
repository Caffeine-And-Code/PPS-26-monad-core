package monad_core.simulator.presentation.chat

import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{ConversationId, UserPrompt}
import scalafx.beans.property.ObjectProperty

import scala.concurrent.ExecutionContext

final class ChatPanelViewModel(
    aiAgent: AiAgent,
    runOnUiThread: (() => Unit) => Unit
)(using executionContext: ExecutionContext):

  private val defaultConversationId = "chat1"

  val state: ObjectProperty[ChatPanelState] =
    ObjectProperty(ChatPanelState.initial)

  def onPromptChange(newPrompt: String): Unit =
    update(ChatPanelActions.onPromptChange(_, newPrompt))

  def onSubmit(): Unit =
    Option.when(state.value.canSend)(state.value.prompt.trim).foreach { prompt =>
      update(ChatPanelActions.onSubmit)

      val askAgentCommand = for {
        userPrompt     <- UserPrompt.from(prompt)
        conversationId <- ConversationId.from(defaultConversationId)
      } yield AskAgentCommand(conversationId, userPrompt)

      askAgentCommand.foreach { command =>
        aiAgent.ask(command).foreach { response =>
          runOnUiThread { () =>
            update(ChatPanelActions.onAgentRespond(_, response))
          }
        }
      }
    }

  def onClearHistory(): Unit =
    if state.value.messages.nonEmpty && !state.value.isWaiting then
      ConversationId.from(defaultConversationId).map(CleanHistoryCommand.apply).foreach { command =>
        update(ChatPanelActions.onHistoryCleaned(_, aiAgent.cleanHistory(command)))
      }

  private def update(action: ChatPanelState => ChatPanelState): Unit =
    state.value = action(state.value)

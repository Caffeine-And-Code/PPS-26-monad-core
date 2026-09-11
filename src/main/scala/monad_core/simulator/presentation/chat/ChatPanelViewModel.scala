package monad_core.simulator.presentation.chat

import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{ConversationId, UserPrompt}
import scalafx.beans.property.ObjectProperty

import scala.concurrent.ExecutionContext

/**
 * Connect the chat panel actions to the state and preserve the state of the chat panel.
 *
 * @param aiAgent agent used to answer prompts and clear history
 * @param runOnUiThread schedules an action on the ScalaFX thread
 * @param executionContext execution context used by asynchronous responses
 */
final class ChatPanelViewModel(
    aiAgent: AiAgent,
    runOnUiThread: (() => Unit) => Unit
)(using executionContext: ExecutionContext):

  private val defaultConversationId = "chat1"

  /** Observable chat state consumed by the panel. */
  val state: ObjectProperty[ChatPanelState] =
    ObjectProperty(ChatPanelState.initial)

  /**
   * Event trigger on user prompt TextField change
   *
   * @param newPrompt current contents of the prompt field
   */
  def onPromptChange(newPrompt: String): Unit =
    update(ChatPanelActions.onPromptChange(_, newPrompt))

  /** Submits the current prompt when the state allows it. */
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

  /** Clears the active conversation when messages exist and no response is pending. */
  def onClearHistory(): Unit =
    if state.value.messages.nonEmpty && !state.value.isWaiting then
      ConversationId.from(defaultConversationId).map(CleanHistoryCommand.apply).foreach { command =>
        update(ChatPanelActions.onHistoryCleaned(_, aiAgent.cleanHistory(command)))
      }

  private def update(action: ChatPanelState => ChatPanelState): Unit =
    state.value = action(state.value)

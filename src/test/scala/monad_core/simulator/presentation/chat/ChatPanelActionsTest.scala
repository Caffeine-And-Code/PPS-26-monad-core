package monad_core.simulator.presentation.chat

import monad_core.simulator.domain.ai.{
  AgentResponse,
  AgentResponseError,
  ConversationId,
  ConversationNotFoundError
}
import org.scalatest.EitherValues.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChatPanelActionsTest extends AnyFunSuite with Matchers:

  private val validPrompt = "Hi Jimmy, how are you?"
  private val agentReply  = AgentResponse("Fine!")

  test("onPromptChange returns the state with the updated prompt"):
    val result = ChatPanelActions.onPromptChange(ChatPanelState.initial, validPrompt)

    result shouldBe ChatPanelState.Ready(Seq.empty, validPrompt)

  test("onSubmit returns a waiting state containing the user message"):
    val state = ChatPanelState.Ready(Seq.empty, validPrompt)

    val result = ChatPanelActions.onSubmit(state)

    result shouldBe ChatPanelState.Waiting(
      Seq(ChatMessage(validPrompt, MessageAuthor.User))
    )

  test("onSubmit leaves the state unchanged when the prompt is empty"):
    val emptyPrompt = "     "
    val state       = ChatPanelState.Ready(Seq.empty, emptyPrompt)

    val result = ChatPanelActions.onSubmit(state)

    result shouldBe state

  test("onAgentRespond adds the assistant message when the response succeeds"):
    val messages = Seq(ChatMessage(validPrompt, MessageAuthor.User))
    val state    = ChatPanelState.Waiting(messages)

    val result = ChatPanelActions.onAgentRespond(state, Right(agentReply))

    result shouldBe
      ChatPanelState.Ready(
        messages :+ ChatMessage(agentReply.response, MessageAuthor.Assistant),
        prompt = ""
      )

  test("onAgentRespond returns an error state when the response fails"):
    val messages = Seq(ChatMessage(validPrompt, MessageAuthor.User))
    val state    = ChatPanelState.Waiting(messages)

    val result = ChatPanelActions.onAgentRespond(state, Left(AgentResponseError("error")))

    result shouldBe
      ChatPanelState.Error(messages, prompt = "", error = "error")

  test("onAgentRespond ignores responses received when state not in waiting"):
    val state = ChatPanelState.initial

    val result = ChatPanelActions.onAgentRespond(state, Right(agentReply))

    result shouldBe state

  test("onHistoryCleaned removes all messages when cleaning succeeds"):
    val state = ChatPanelState.Ready(
      Seq(ChatMessage(validPrompt, MessageAuthor.User)),
      validPrompt
    )

    val result = ChatPanelActions.onHistoryCleaned(state, Right(()))

    result shouldBe ChatPanelState.Ready(Vector.empty, validPrompt)

  test("onHistoryCleaned preserves messages and exposes the error when aiAgent clean fails"):
    val conversationId = ConversationId.from("chat1").value
    val error          = ConversationNotFoundError(conversationId)
    val messages       = Seq(ChatMessage(validPrompt, MessageAuthor.User))
    val state          = ChatPanelState.Ready(messages, validPrompt)

    val result = ChatPanelActions.onHistoryCleaned(state, Left(error))

    result shouldBe ChatPanelState.Error(messages, validPrompt, error.message)

package monad_core.simulator.presentation.chat

import monad_core.simulator.application.AgentService
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChatPanelActionsTest extends AnyFunSuite with Matchers with MockFactory:

  val validPrompt: String = "Hi Jimmy, how are you?"
  val agentReply: String = "Fine!"

  test("onPromptChange calls renderer with the updated state"):
    val mockedRenderer = mockFunction[ChatPanelState, Unit]
    val actions = ChatPanelActions(ChatPanelState.initial, mockedRenderer)
    val expectedState = ChatPanelState.Ready(Seq.empty, validPrompt)

    mockedRenderer.expects(expectedState).once()

    val result = actions.onPromptChange(validPrompt)

    result shouldBe ChatPanelActions(expectedState, mockedRenderer)

  test("onSubmit calls renderer with waiting state and then with ready state when agent service returns a reply when prompt is not empty"):
    val mockedRenderer = mockFunction[ChatPanelState, Unit]
    val mockedAgentService: AgentService = mock[AgentService]
    val actions = ChatPanelActions(ChatPanelState.Ready(Seq.empty, validPrompt), mockedRenderer)
    val finalState = ChatPanelState.Ready(Seq(
      ChatMessage(validPrompt, MessageAuthor.User),
      ChatMessage(agentReply, MessageAuthor.Assistant)
    ), validPrompt)
    val stateInWaiting = ChatPanelState.Waiting(Seq(
      ChatMessage(validPrompt, MessageAuthor.User))
    )

    inSequence{
      mockedRenderer.expects(stateInWaiting).once()
      mockedRenderer.expects(finalState).once()
    }
    mockedAgentService.ask.expects(validPrompt).returns(Right(agentReply))

    val result = actions.onSubmit()(using mockedAgentService)

    result shouldBe ChatPanelActions(finalState, mockedRenderer)
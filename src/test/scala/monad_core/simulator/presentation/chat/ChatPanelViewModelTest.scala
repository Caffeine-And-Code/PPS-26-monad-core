package monad_core.simulator.presentation.chat

import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand}
import monad_core.simulator.application.{AgentCallError, AgentService}
import monad_core.simulator.domain.ai.{AgentResponse, AgentResponseError, ConversationId, UserPrompt}
import monad_core.simulator.errors.BaseError
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues.*

import scala.concurrent.{ExecutionContext, Promise}

class ChatPanelViewModelTest extends AnyFunSuite with Matchers with MockFactory:

  given ExecutionContext = ExecutionContext.parasitic

  private val validPrompt = "Hello"
  private val validAnswer = AgentResponse("Hi!", 0)
  private val emptyPrompt = ""
  private val userMessage = ChatMessage(validPrompt, MessageAuthor.User)
  private val executeInUIThread: (() => Unit) => Unit = action => action()

  test("onPromptChange changes the state containing the new prompt"):
    val aiAgent = mock[AiAgent]
    val viewModel = new ChatPanelViewModel(aiAgent, executeInUIThread)

    viewModel.onPromptChange(validPrompt)

    viewModel.state.value shouldBe ChatPanelState.Ready(Seq.empty, validPrompt)

  test("onSubmit change state to waiting and asks the agent once"):
    val pendingResponse = Promise[Either[AgentResponseError, AgentResponse]]()
    val aiAgent = mock[AiAgent]
    val askAgentCommand = AskAgentCommand(ConversationId.from("chat1").value, UserPrompt.from(validPrompt).value)
    aiAgent.ask.expects(askAgentCommand).returning(pendingResponse.future).once()
    val viewModel = new ChatPanelViewModel(aiAgent, executeInUIThread)
    viewModel.onPromptChange(validPrompt)

    viewModel.onSubmit()

    viewModel.state.value shouldBe ChatPanelState.Waiting(Seq(userMessage))

  test("the agent response is published using the UI thread"):
    val pendingResponse = Promise[Either[AgentResponseError, AgentResponse]]()
    val aiAgent = mock[AiAgent]
    val uiExecutor = mockFunction[() => Unit, Unit]
    val viewModel = new ChatPanelViewModel(aiAgent, uiExecutor)
    val askAgentCommand = AskAgentCommand(ConversationId.from("chat1").value, UserPrompt.from(validPrompt).value)

    aiAgent.ask.expects(askAgentCommand).returning(pendingResponse.future).once()
    uiExecutor.expects(*).onCall((action: () => Unit) => action()).once()

    viewModel.onPromptChange(validPrompt)
    viewModel.onSubmit()

    pendingResponse.success(Right(validAnswer))

    viewModel.state.value shouldBe ChatPanelState.Ready(
      Seq(userMessage, ChatMessage(validAnswer.response, MessageAuthor.Assistant)),
      emptyPrompt
    )

  test("when agent response fails the states become an error state"):
    val error = AgentResponseError("Agent unavailable")
    val pendingResponse = Promise[Either[AgentResponseError, AgentResponse]]()
    val aiAgent = mock[AiAgent]
    val viewModel = new ChatPanelViewModel(aiAgent, executeInUIThread)
    val askAgentCommand = AskAgentCommand(ConversationId.from("chat1").value, UserPrompt.from(validPrompt).value)

    aiAgent.ask.expects(askAgentCommand).returning(pendingResponse.future).once()

    viewModel.onPromptChange(validPrompt)
    viewModel.onSubmit()

    pendingResponse.success(Left(error))

    viewModel.state.value shouldBe ChatPanelState.Error(
      Seq(userMessage),
      emptyPrompt,
      error.message
    )

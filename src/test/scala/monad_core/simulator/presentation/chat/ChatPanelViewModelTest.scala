package monad_core.simulator.presentation.chat

import monad_core.simulator.application.{AgentCallError, AgentService}
import monad_core.simulator.errors.BaseError
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Promise}

class ChatPanelViewModelTest extends AnyFunSuite with Matchers with MockFactory:

  given ExecutionContext = ExecutionContext.parasitic

  private val validPrompt = "Hello"
  private val validAnswer = "Hi!"
  private val emptyPrompt = ""
  private val userMessage = ChatMessage(validPrompt, MessageAuthor.User)
  private val executeInUIThread: (() => Unit) => Unit = action => action()

  test("onPromptChange changes the state containing the new prompt"):
    val agentService = mock[AgentService]
    val viewModel = new ChatPanelViewModel(agentService, executeInUIThread)

    viewModel.onPromptChange(validPrompt)

    viewModel.state.value shouldBe ChatPanelState.Ready(Seq.empty, validPrompt)

  test("onSubmit change state to waiting and asks the agent once"):
    val pendingResponse = Promise[Either[BaseError, String]]()
    val agentService = mock[AgentService]
    agentService.ask.expects(validPrompt).returning(pendingResponse.future).once()
    val viewModel = new ChatPanelViewModel(agentService, executeInUIThread)
    viewModel.onPromptChange(validPrompt)

    viewModel.onSubmit()

    viewModel.state.value shouldBe ChatPanelState.Waiting(Seq(userMessage))

  test("the agent response is published using the UI thread"):
    val pendingResponse = Promise[Either[BaseError, String]]()
    val agentService = mock[AgentService]
    val uiExecutor = mockFunction[() => Unit, Unit]
    val viewModel = new ChatPanelViewModel(agentService, uiExecutor)

    agentService.ask.expects(validPrompt).returning(pendingResponse.future).once()
    uiExecutor.expects(*).onCall((action: () => Unit) => action()).once()

    viewModel.onPromptChange(validPrompt)
    viewModel.onSubmit()

    pendingResponse.success(Right(validAnswer))

    viewModel.state.value shouldBe ChatPanelState.Ready(
      Seq(userMessage, ChatMessage(validAnswer, MessageAuthor.Assistant)),
      emptyPrompt
    )

  test("when agent response fails the states become an error state"):
    val error = AgentCallError("Agent unavailable")
    val pendingResponse = Promise[Either[BaseError, String]]()
    val agentService = mock[AgentService]
    val viewModel = new ChatPanelViewModel(agentService, executeInUIThread)

    agentService.ask.expects(validPrompt).returning(pendingResponse.future).once()

    viewModel.onPromptChange(validPrompt)
    viewModel.onSubmit()

    pendingResponse.success(Left(error))

    viewModel.state.value shouldBe ChatPanelState.Error(
      Seq(userMessage),
      emptyPrompt,
      error.message
    )

package integrations.monad_core.simulator.presentation.panels

import integrations.support.ScalaFxTest
import monad_core.simulator.application.ai.{AiAgent, AskAgentCommand, CleanHistoryCommand}
import monad_core.simulator.domain.ai.{AgentResponse, AgentResponseError, ConversationId, UserPrompt}
import monad_core.simulator.presentation.panels.AiModelChatPanel
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scalafx.scene.layout.VBox as ScalaFxVBox
import org.scalatest.EitherValues.*

import scala.concurrent.{ExecutionContext, Future, Promise}

class AiModelChatPanelTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with ScalaFxTest:

  given ExecutionContext = ExecutionContext.parasitic

  test("submit is disabled until a prompt is entered"):
    val panel = buildPanel(mock[AiAgent])

    onFxThread {
      submitButton(panel).isDisable shouldBe true
    }

  test("submit is disabled when prompt is empty"):
    val panel = buildPanel(mock[AiAgent])

    onFxThread {
      val prompt = promptField(panel)
      val submit = submitButton(panel)

      prompt.setText("Hello")
      submit.isDisable shouldBe false

      prompt.setText("")
      submit.isDisable shouldBe true
    }

  test("after prompt submits, it render the user and the assistant messages"):
    val aiAgent = mock[AiAgent]
    val prompt = "Hello"
    val response = "Hi!"
    val userPrompt = UserPrompt.from(prompt).value
    val conversationId = ConversationId.from("chat1").value
    val panel = buildPanel(aiAgent)

    aiAgent.ask.expects(
      AskAgentCommand(conversationId, userPrompt)
    ).returning(Future.successful(Right(AgentResponse(response, 0)))).once()

    onFxThread {
      promptField(panel).setText(prompt)
      submitButton(panel).fire()
    }
    drainFxQueue()

    onFxThread {
      messageTexts(panel) should contain allOf (prompt, response)
    }

  test("it renders the error message when the AI ask request fails"):
    val aiAgent = mock[AiAgent]
    val prompt = "Hello"
    val error = AgentResponseError("Agent unavailable")
    val conversationId = ConversationId.from("chat1").value
    val panel = buildPanel(aiAgent)

    aiAgent.ask
      .expects(AskAgentCommand(conversationId, UserPrompt.from(prompt).value))
      .returning(Future.successful(Left(error)))
      .once()

    onFxThread {
      promptField(panel).setText(prompt)
      submitButton(panel).fire()
    }
    drainFxQueue()

    onFxThread {
      messageTexts(panel) should contain allOf (prompt, s"Error: ${error.message}")
    }

  test("prompt and submit are disabled while waiting for the AI response"):
    val pendingResponse = Promise[Either[AgentResponseError, AgentResponse]]()
    val aiAgent = mock[AiAgent]
    val panel = buildPanel(aiAgent)

    aiAgent.ask.expects(*).returning(pendingResponse.future).once()

    onFxThread {
      promptField(panel).setText("Hello")
      submitButton(panel).fire()

      promptField(panel).isDisable shouldBe true
      submitButton(panel).isDisable shouldBe true
    }

  test("clear is disabled when there are no messages"):
    val panel = buildPanel(mock[AiAgent])

    onFxThread {
      clearButton(panel).isDisable shouldBe true
    }

  test("clear removes all messages and cleans the AI history"):
    val aiAgent = mock[AiAgent]
    val prompt = "Hello"
    val response = "Hi!"
    val conversationId = ConversationId.from("chat1").value
    val panel = buildPanel(aiAgent)

    aiAgent.ask
      .expects(AskAgentCommand(conversationId, UserPrompt.from(prompt).value))
      .returning(Future.successful(Right(AgentResponse(response, 0))))
      .once()
    aiAgent.cleanHistory
      .expects(CleanHistoryCommand(conversationId))
      .returning(Right(()))
      .once()

    onFxThread {
      promptField(panel).setText(prompt)
      submitButton(panel).fire()
    }
    drainFxQueue()

    onFxThread {
      clearButton(panel).isDisable shouldBe false
      clearButton(panel).fire()

      messageTexts(panel) shouldBe empty
      clearButton(panel).isDisable shouldBe true
    }

  private def buildPanel(aiAgent: AiAgent): ScalaFxVBox =
    onFxThread {
      AiModelChatPanel.build(aiAgent).value
    }

  private def promptField(panel: ScalaFxVBox): javafx.scene.control.TextField =
    panel.delegate.lookup("#chat-prompt").asInstanceOf[javafx.scene.control.TextField]

  private def submitButton(panel: ScalaFxVBox): javafx.scene.control.Button =
    panel.delegate.lookup("#chat-send").asInstanceOf[javafx.scene.control.Button]

  private def clearButton(panel: ScalaFxVBox): javafx.scene.control.Button =
    panel.delegate.lookup("#chat-clear").asInstanceOf[javafx.scene.control.Button]

  private def messageTexts(panel: ScalaFxVBox): Seq[String] =
    val messages = panel.delegate
      .lookup("#chat-scroll")
      .asInstanceOf[javafx.scene.control.ScrollPane]
      .getContent
    descendants(messages).collect {
      case label: javafx.scene.control.Label => label.getText
    }

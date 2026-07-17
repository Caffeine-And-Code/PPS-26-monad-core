package monad_core.simulator.presentation.chat

import org.scalactic.Prettifier.default
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChatPanelStateTest extends AnyFunSuite with Matchers:

  val emptyPrompt = ""
  val validPrompt = "Hi Jimmy how are you?"
  val emptyMessages: Seq[ChatMessage] = Seq.empty[ChatMessage]
  val validMessages: Seq[ChatMessage] = Seq(
    ChatMessage(validPrompt, MessageAuthor.User),
    ChatMessage("Jimmy's response", MessageAuthor.Assistant)
  )
  val validError = "error"

  test("initial chat state is ready with no messages and an empty prompt"):
    ChatPanelState.initial shouldBe ChatPanelState.Ready(emptyMessages, emptyPrompt)

  test("on state ready, messages returns the messages"):
    ChatPanelState.Ready(validMessages, emptyPrompt).messages shouldBe validMessages

  test("on state ready, prompt returns the prompt"):
    ChatPanelState.Ready(emptyMessages, validPrompt).prompt shouldBe validPrompt

  test("on state ready canSend is true with a not empty prompt"):
    ChatPanelState.Ready(emptyMessages, validPrompt).canSend shouldBe true

  test("on state ready canSend is false with a empty prompt"):
    ChatPanelState.Ready(emptyMessages, emptyPrompt).canSend shouldBe false
    ChatPanelState.Ready(emptyMessages, "    " + emptyPrompt).canSend shouldBe false

  test("on state waiting messages returns the messages"):
    ChatPanelState.Waiting(validMessages).messages shouldBe validMessages

  test("on state waiting prompt is empty"):
    ChatPanelState.Waiting(emptyMessages).prompt shouldBe empty

  test("on state waiting canSend is false"):
    ChatPanelState.Waiting(emptyMessages).canSend shouldBe false

  test("on state error messages returns the messages"):
    ChatPanelState.Error(validMessages, emptyPrompt, validError).messages shouldBe validMessages

  test("on state error prompt returns the prompt"):
    ChatPanelState.Error(emptyMessages, validPrompt, validError).prompt shouldBe validPrompt

  test("on state error canSend is true with a non-blank prompt"):
    ChatPanelState.Error(emptyMessages, validPrompt, validError).canSend shouldBe true

  test("on state error canSend is false with a empty prompt"):
    ChatPanelState.Error(emptyMessages, emptyPrompt, validError).canSend shouldBe false
    ChatPanelState.Error(emptyMessages, "    " + emptyPrompt, validError).canSend shouldBe false
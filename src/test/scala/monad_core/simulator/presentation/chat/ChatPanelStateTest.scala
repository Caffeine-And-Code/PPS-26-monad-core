package monad_core.simulator.presentation.chat

import dev.langchain4j.data.message.UserMessage
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

  test("on state success calling setPrompt sets the prompt"):
    val state = ChatPanelState.Ready(emptyMessages, emptyPrompt)

    val result = state.setPrompt(validPrompt)

    result.prompt shouldBe validPrompt

  test("on state waiting calling setPrompt do nothing"):
    val state = ChatPanelState.Waiting(emptyMessages)

    val result = state.setPrompt(validPrompt)

    result shouldBe state

  test("on state error calling setPrompt sets the prompt"):
    val state = ChatPanelState.Error(emptyMessages, emptyPrompt, validError)

    val result = state.setPrompt(validPrompt)

    result.prompt shouldBe validPrompt

  test("calling toWaiting converts the state in waiting state"):
    val ready = ChatPanelState.Ready(emptyMessages, emptyPrompt)
    val waiting = ChatPanelState.Waiting(emptyMessages)
    val error = ChatPanelState.Error(emptyMessages, emptyPrompt, validError)

    val readyResult = ready.toWaiting
    val waitingResult = waiting.toWaiting
    val errorResult = error.toWaiting

    readyResult shouldBe waiting
    waitingResult shouldBe waiting
    errorResult shouldBe waiting

  test("calling toReady converts the state in ready state"):
    val ready = ChatPanelState.Ready(emptyMessages, validPrompt)
    val waiting = ChatPanelState.Waiting(emptyMessages)
    val error = ChatPanelState.Error(emptyMessages, validPrompt, validError)

    val readyResult = ready.toReady
    val waitingResult = waiting.toReady
    val errorResult = error.toReady

    readyResult shouldBe ready
    waitingResult shouldBe ChatPanelState.Ready(emptyMessages, emptyPrompt)
    errorResult shouldBe ready

  test("calling toError converts the state in error state"):
    val ready = ChatPanelState.Ready(emptyMessages, emptyPrompt)
    val waiting = ChatPanelState.Waiting(emptyMessages)
    val error = ChatPanelState.Error(emptyMessages, emptyPrompt, validError)

    val readyResult = ready.toError(validError)
    val waitingResult = waiting.toError(validError)
    val errorResult = error.toError(validError)

    readyResult shouldBe error
    waitingResult shouldBe error
    errorResult shouldBe error

  test("on ready state, calling addMessage adds a message"):
    val message = ChatMessage(validPrompt, MessageAuthor.User)
    val ready = ChatPanelState.Ready(emptyMessages, emptyPrompt)
    val expected = ChatPanelState.Ready(Seq(message), emptyPrompt)

    val result = ready.addMessage(message)

    result shouldBe expected

  test("on waiting state, calling addMessage adds a message"):
    val message = ChatMessage(validPrompt, MessageAuthor.User)
    val waiting = ChatPanelState.Waiting(emptyMessages)
    val expected = ChatPanelState.Waiting(Seq(message))

    val result = waiting.addMessage(message)

    result shouldBe expected

  test("on error state, calling addMessage adds a message"):
    val message = ChatMessage(validPrompt, MessageAuthor.User)
    val error = ChatPanelState.Error(emptyMessages, emptyPrompt, validError)
    val expected = ChatPanelState.Error(Seq(message), emptyPrompt, validError)

    val result = error.addMessage(message)

    result shouldBe expected
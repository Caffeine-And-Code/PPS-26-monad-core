package monad_core.simulator.presentation.chat

import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ChatPanelActionsTest extends AnyFunSuite with Matchers with MockFactory:

  test("onPromptChange calls renderer with the updated state"):
    val mockedRenderer = mockFunction[ChatPanelState, Unit]
    val prompt = "Hi Jimmy how are you?"
    val actions = ChatPanelActions(ChatPanelState.initial, mockedRenderer)
    val expectedState = ChatPanelState.Ready(Seq.empty, prompt)

    mockedRenderer.expects(expectedState).once()

    val result = actions.onPromptChange(prompt)

    result shouldBe ChatPanelActions(expectedState, mockedRenderer)

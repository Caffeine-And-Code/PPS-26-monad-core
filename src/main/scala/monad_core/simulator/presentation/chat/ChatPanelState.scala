package monad_core.simulator.presentation.chat

enum MessageAuthor:
  case User, Assistant

case class ChatMessage(content: String, author: MessageAuthor)

enum ChatPanelState:
  case Ready(messages: Seq[ChatMessage], prompt: String)
  case Waiting(messages: Seq[ChatMessage])
  case Error(messages: Seq[ChatMessage], prompt: String, error: String)

object ChatPanelState:
  val initial: ChatPanelState = Ready(messages = Vector.empty, prompt = "")

extension (model: ChatPanelState)
  def messages: Seq[ChatMessage] =
    model match
      case ChatPanelState.Ready(messages, _) => messages
      case ChatPanelState.Waiting(messages) => messages
      case ChatPanelState.Error(messages, _, _) => messages

  def prompt: String =
    model match
      case ChatPanelState.Ready(_, prompt) => prompt
      case ChatPanelState.Error(_, prompt, _) => prompt
      case ChatPanelState.Waiting(_) => ""

  def canSend: Boolean =
    model match
      case ChatPanelState.Ready(_, prompt) => prompt.trim.nonEmpty
      case ChatPanelState.Error(_, prompt, _) => prompt.trim.nonEmpty
      case ChatPanelState.Waiting(_) => false
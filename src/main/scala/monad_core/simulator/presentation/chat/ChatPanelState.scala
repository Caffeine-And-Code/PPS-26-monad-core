package monad_core.simulator.presentation.chat

import monad_core.simulator.presentation.chat.ChatPanelState.{Error, Ready, Waiting}

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
      case ChatPanelState.Ready(messages, _)    => messages
      case ChatPanelState.Waiting(messages)     => messages
      case ChatPanelState.Error(messages, _, _) => messages

  def prompt: String =
    model match
      case ChatPanelState.Ready(_, prompt)    => prompt
      case ChatPanelState.Error(_, prompt, _) => prompt
      case ChatPanelState.Waiting(_)          => ""

  def canSend: Boolean =
    model match
      case ChatPanelState.Ready(_, prompt)    => prompt.trim.nonEmpty
      case ChatPanelState.Error(_, prompt, _) => prompt.trim.nonEmpty
      case ChatPanelState.Waiting(_)          => false

  def setPrompt(newPrompt: String): ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, _)        => Ready(messages, newPrompt)
      case ChatPanelState.Error(messages, _, error) => Error(messages, newPrompt, error)
      case ChatPanelState.Waiting(messages)         => Waiting(messages)

  def toWaiting: ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, _)    => ChatPanelState.Waiting(messages)
      case ChatPanelState.Waiting(messages)     => ChatPanelState.Waiting(messages)
      case ChatPanelState.Error(messages, _, _) => ChatPanelState.Waiting(messages)

  def toReady: ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, prompt)    => ChatPanelState.Ready(messages, prompt)
      case ChatPanelState.Waiting(messages)          => ChatPanelState.Ready(messages, "")
      case ChatPanelState.Error(messages, prompt, _) => ChatPanelState.Ready(messages, prompt)

  def toError(error: String): ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, prompt) => ChatPanelState.Error(messages, prompt, error)
      case ChatPanelState.Waiting(messages)       => ChatPanelState.Error(messages, "", error)
      case ChatPanelState.Error(messages, prompt, error) =>
        ChatPanelState.Error(messages, prompt, error)

  def addMessage(chatMessage: ChatMessage): ChatPanelState =
    model match
      case panel: ChatPanelState.Ready   => panel.copy(messages = messages :+ chatMessage)
      case panel: ChatPanelState.Waiting => panel.copy(messages = messages :+ chatMessage)
      case panel: ChatPanelState.Error   => panel.copy(messages = messages :+ chatMessage)

  def isWaiting: Boolean =
    model match
      case Waiting(_) => true
      case _          => false

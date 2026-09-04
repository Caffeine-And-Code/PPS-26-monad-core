package monad_core.simulator.presentation.chat

import monad_core.simulator.presentation.chat.ChatPanelState.{Error, Ready, Waiting}

/** Identifies the participant that produced a chat message. */
enum MessageAuthor:
  case User, Assistant

/**
 * Message displayed in the chat history.
 *
 * @param content message text
 * @param author participant that produced the message
 */
case class ChatMessage(content: String, author: MessageAuthor)

/** State of the AI chat panel. */
enum ChatPanelState:

  /**
   * User can send a message.
   *
   * @param messages completed messages
   * @param prompt editable prompt
   */
  case Ready(messages: Seq[ChatMessage], prompt: String)

  /**
   * Waiting for agent response.
   *
   * @param messages messages completed before the pending response
   */
  case Waiting(messages: Seq[ChatMessage])

  /**
   * There has been an error after an actions has been taken.
   *
   * @param messages completed messages
   * @param prompt editable prompt
   * @param error error shown by the panel
   */
  case Error(messages: Seq[ChatMessage], prompt: String, error: String)

/** Initial value and transformations for [[ChatPanelState]]. */
object ChatPanelState:

  /** Empty state ready to accept a prompt. */
  val initial: ChatPanelState = Ready(messages = Vector.empty, prompt = "")

extension (model: ChatPanelState)

  /** @return messages stored by the state */
  def messages: Seq[ChatMessage] =
    model match
      case ChatPanelState.Ready(messages, _)    => messages
      case ChatPanelState.Waiting(messages)     => messages
      case ChatPanelState.Error(messages, _, _) => messages

  /** @return current editable prompt, or an empty string while waiting */
  def prompt: String =
    model match
      case ChatPanelState.Ready(_, prompt)    => prompt
      case ChatPanelState.Error(_, prompt, _) => prompt
      case ChatPanelState.Waiting(_)          => ""

  /** @return whether the state contains a non-blank prompt that can be submitted */
  def canSend: Boolean =
    model match
      case ChatPanelState.Ready(_, prompt)    => prompt.trim.nonEmpty
      case ChatPanelState.Error(_, prompt, _) => prompt.trim.nonEmpty
      case ChatPanelState.Waiting(_)          => false

  /**
   * Set a new prompt in the textbox when in ready or error state.
   *
   * @param newPrompt replacement prompt
   * @return updated state
   */
  def setPrompt(newPrompt: String): ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, _)        => Ready(messages, newPrompt)
      case ChatPanelState.Error(messages, _, error) => Error(messages, newPrompt, error)
      case ChatPanelState.Waiting(messages)         => Waiting(messages)

  /** @return waiting state preserving the messages */
  def toWaiting: ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, _)    => ChatPanelState.Waiting(messages)
      case ChatPanelState.Waiting(messages)     => ChatPanelState.Waiting(messages)
      case ChatPanelState.Error(messages, _, _) => ChatPanelState.Waiting(messages)

  /** @return ready state with the current messages and available prompt */
  def toReady: ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, prompt)    => ChatPanelState.Ready(messages, prompt)
      case ChatPanelState.Waiting(messages)          => ChatPanelState.Ready(messages, "")
      case ChatPanelState.Error(messages, prompt, _) => ChatPanelState.Ready(messages, prompt)

  /**
   * Set state to error state
   *
   * @param error error description
   * @return error state preserving available input
   * */
  def toError(error: String): ChatPanelState =
    model match
      case ChatPanelState.Ready(messages, prompt) => ChatPanelState.Error(messages, prompt, error)
      case ChatPanelState.Waiting(messages)       => ChatPanelState.Error(messages, "", error)
      case ChatPanelState.Error(messages, prompt, error) =>
        ChatPanelState.Error(messages, prompt, error)

  /**
   * Add a message to message list
   *
   * @param chatMessage message to append
   * @return state containing the appended message
   */
  def addMessage(chatMessage: ChatMessage): ChatPanelState =
    model match
      case panel: ChatPanelState.Ready   => panel.copy(messages = messages :+ chatMessage)
      case panel: ChatPanelState.Waiting => panel.copy(messages = messages :+ chatMessage)
      case panel: ChatPanelState.Error   => panel.copy(messages = messages :+ chatMessage)

  /** @return `true` only for a waiting state */
  def isWaiting: Boolean =
    model match
      case Waiting(_) => true
      case _          => false

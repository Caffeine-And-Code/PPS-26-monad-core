package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

/**
 * Identifies the provider and model used by an AI agent.
 *
 * @param provider provider display name
 * @param modelName model identifier
 */
case class AgentInfo(
    provider: String,
    modelName: String
)

/** Error returned when an empty provider name is supplied. */
case class InvalidProviderName() extends BaseError("invalid provider name")

/** Error returned when an empty model name is supplied. */
case class InvalidModelName() extends BaseError("invalid model name")

/** Factory for [[AgentInfo]]. */
object AgentInfo:

  /**
   * Creates agent information from string names.
   *
   * @param provider provider name, should not be empty
   * @param modelName model name, should not be empty
   * @return agent information, or the validation error for the first blank value
   */
  def from(provider: String, modelName: String): Either[BaseError, AgentInfo] =
    (provider.trim, modelName.trim) match
      case ("", _) => Left(InvalidProviderName())
      case (_, "") => Left(InvalidModelName())
      case (providerTrimmed, modelNameTrimmed) =>
        Right(AgentInfo(providerTrimmed, modelNameTrimmed))

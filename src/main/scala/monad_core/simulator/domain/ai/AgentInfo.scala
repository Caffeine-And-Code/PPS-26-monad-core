package monad_core.simulator.domain.ai

import monad_core.simulator.errors.BaseError

case class AgentInfo (
                    provider: String,
                    modelName: String
                    )

case class InvalidProviderName() extends BaseError("invalid provider name")
case class InvalidModelName() extends BaseError("invalid model name")

object AgentInfo:

  def from(provider:String, modelName: String): Either[BaseError, AgentInfo] =
    if provider.trim.isEmpty then
      Left(InvalidProviderName())
    else if modelName.trim.isEmpty then
      Left(InvalidModelName())
    else
      Right(AgentInfo(provider, modelName))
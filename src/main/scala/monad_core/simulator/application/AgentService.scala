package monad_core.simulator.application

import dev.langchain4j.model.chat.ChatModel
import dev.langchain4j.model.ollama.OllamaChatModel
import monad_core.simulator.errors.BaseError

import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Try

case class ModelInfo(provider:String, model: String)

trait AgentService:
  def ask(message: String): Future[Either[BaseError, String]]
  def modelInfo:ModelInfo

object AgentService:

  final case class OllamaConfig(
      baseUrl: String = "http://localhost:11434",
      modelName: String = "gemma4:e2b"
  )

  def apply(model: ChatModel)(using executionContext: ExecutionContext): AgentService = new AgentService:
    override def ask(message: String): Future[Either[AgentCallError, String]] =
      Future {
        blocking {
          Try(model.chat(message))
            .toEither
            .left
            .map(error => AgentCallError(Option(error.getMessage).getOrElse(error.toString)))
        }
      }

    override def modelInfo:ModelInfo =
      ModelInfo(model.provider.toString, model.defaultRequestParameters().modelName())

  def ollama(config: OllamaConfig = OllamaConfig())(using ExecutionContext): AgentService =
    val model = OllamaChatModel.builder()
      .baseUrl(config.baseUrl)
      .modelName(config.modelName)
      .build()

    AgentService(model)

  given defaultAgentService(using ExecutionContext): AgentService = ollama()

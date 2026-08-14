package monad_core.simulator.presentation.agent_evaluation

case class AgentEvaluationArguments(
    testModelUrl: String,
    testModel: String,
    judgeModelUrl: String,
    judgeModel: String
)

object AgentEvaluationArguments:

  val agentModelUrlArgument = "--agent-model-url"
  val agentModelArgument    = "--agent-model"
  val judgeModelUrlArgument = "--judge-model-url"
  val judgeModelArgument    = "--judge-model"

  def parse(args: Array[String]): AgentEvaluationArguments =
    args
      .sliding(2)
      .foldLeft(defaultValue)((actual, arguments) =>
        if arguments.length == 2 then
          updatedAgentEvaluationArgumentsActualValue(actual, arguments.head, arguments.last)
        else actual
      )

  private def updatedAgentEvaluationArgumentsActualValue(
      actual: AgentEvaluationArguments,
      argument: String,
      value: String
  ): AgentEvaluationArguments =
    argument match
      case `agentModelUrlArgument` => actual.copy(testModelUrl = value)
      case `agentModelArgument`    => actual.copy(testModel = value)
      case `judgeModelUrlArgument` => actual.copy(judgeModelUrl = value)
      case `judgeModelArgument`    => actual.copy(judgeModel = value)
      case _                       => actual

  private def defaultValue: AgentEvaluationArguments =
    AgentEvaluationArguments(
      testModelUrl = "http://localhost:11434",
      testModel = "gemma4:e4b",
      judgeModelUrl = "http://localhost:11434",
      judgeModel = "gemma4:e4b"
    )

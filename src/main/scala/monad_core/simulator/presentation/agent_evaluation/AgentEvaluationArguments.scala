package monad_core.simulator.presentation.agent_evaluation

/**
 * Model endpoints and names used for evaluation.
 *
 * @param testModelUrl endpoint of the agent under test
 * @param testModel model under test
 * @param judgeModelUrl endpoint of the judge model
 * @param judgeModel judge model
 */
case class AgentEvaluationArguments(
    testModelUrl: String,
    testModel: String,
    judgeModelUrl: String,
    judgeModel: String
)

/** Command-line names and parser for [[AgentEvaluationArguments]]. */
object AgentEvaluationArguments:

  /** Argument that overrides the tested agent endpoint. */
  val agentModelUrlArgument = "--agent-model-url"

  /** Argument that overrides the tested agent model. */
  val agentModelArgument = "--agent-model"

  /** Argument that overrides the judge endpoint. */
  val judgeModelUrlArgument = "--judge-model-url"

  /** Argument that overrides the judge model. */
  val judgeModelArgument = "--judge-model"

  /**
   * Parse the arguments list to a [[AgentEvaluationArguments]]
   *
   * @param args command-line arguments
   * @return parsed values combined with defaults for absent options
   */
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

package monad_core.simulator.presentation.agent_evaluation

case class AgentEvaluationArguments(
                                   testModelUrl: String,
                                   testModel: String,
                                   judgeModelUrl: String,
                                   judgeModel: String
                                   )

object AgentEvaluationArguments:

  val testModelUrlArgument = "--test-model-url"
  val modelUrlArgument     = "--model-url"
  val testJudgeUrlArgument = "--test-judge-url"
  val judgeUrlArgument     = "--judge-url"

  def parse(args: Array[String]): AgentEvaluationArguments = {
    args.sliding(2).foldLeft(defaultValue)((actual, arguments) =>
      if arguments.length == 2 then
        updatedAgentEvaluationArgumentsActualValue(actual, arguments.head, arguments.last)
      else actual
    )
  }

  private def updatedAgentEvaluationArgumentsActualValue(actual: AgentEvaluationArguments, argument: String, value: String): AgentEvaluationArguments =
    argument match
      case `testModelUrlArgument` => actual.copy(testModelUrl = value)
      case `modelUrlArgument` => actual.copy(testModel = value)
      case `testJudgeUrlArgument` => actual.copy(judgeModelUrl = value)
      case `judgeUrlArgument` => actual.copy(judgeModel = value)
      case _ => actual

  private def defaultValue: AgentEvaluationArguments =
    AgentEvaluationArguments(
      testModelUrl = "http://localhost:11434",
      testModel = "gemma4:e4b",
      judgeModelUrl = "http://localhost:11434",
      judgeModel = "gemma4:e4b"
    )

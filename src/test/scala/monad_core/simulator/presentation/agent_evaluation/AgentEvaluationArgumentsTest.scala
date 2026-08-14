package monad_core.simulator.presentation.agent_evaluation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AgentEvaluationArgumentsTest extends AnyFunSuite with Matchers:

  private val defaultModelUrl = "http://localhost:11434"
  private val defaultModel    = "gemma4:e4b"

  test("can create AgentEvaluationArguments"):
    val testModelUrl  = "http://test-model"
    val testModel     = "test-model"
    val judgeModelUrl = "http://judge-model"
    val judgeModel    = "judge-model"

    val result = AgentEvaluationArguments(testModelUrl, testModel, judgeModelUrl, judgeModel)

    result.testModelUrl shouldBe testModelUrl
    result.testModel shouldBe testModel
    result.judgeModelUrl shouldBe judgeModelUrl
    result.judgeModel shouldBe judgeModel

  test("can parse default AgentEvaluationArguments"):
    val result = AgentEvaluationArguments.parse(Array.empty)

    result shouldBe AgentEvaluationArguments(
      testModelUrl = defaultModelUrl,
      testModel = defaultModel,
      judgeModelUrl = defaultModelUrl,
      judgeModel = defaultModel
    )

  test("can parse AgentEvaluationArguments from arguments"):
    val testModelUrl  = "http://test-model"
    val testModel     = "test-model"
    val judgeModelUrl = "http://judge-model"
    val judgeModel    = "judge-model"
    val args = Array(
      AgentEvaluationArguments.testModelUrlArgument,
      testModelUrl,
      AgentEvaluationArguments.modelUrlArgument,
      testModel,
      AgentEvaluationArguments.testJudgeUrlArgument,
      judgeModelUrl,
      AgentEvaluationArguments.judgeUrlArgument,
      judgeModel
    )

    val result = AgentEvaluationArguments.parse(args)

    result shouldBe AgentEvaluationArguments(testModelUrl, testModel, judgeModelUrl, judgeModel)

  test("unknown arguments are ignored"):
    val unknownArgument = "--unknown"
    val unknownValue    = "unknown"

    val result = AgentEvaluationArguments.parse(Array(unknownArgument, unknownValue))

    result shouldBe AgentEvaluationArguments(
      testModelUrl = defaultModelUrl,
      testModel = defaultModel,
      judgeModelUrl = defaultModelUrl,
      judgeModel = defaultModel
    )

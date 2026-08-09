package monad_core.simulator.infrastructure.ai.agent_evaluator.dataset

import monad_core.simulator.application.ai.AgentEvaluationDataset
import monad_core.simulator.domain.ai.agent_evaluation.AgentEvaluationLanguage
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HardcodedAgentEvaluationDatasetTest extends AnyFunSuite with Matchers:

  private val dataset: AgentEvaluationDataset = HardcodedAgentEvaluationDataset

  test("hardcoded agent evaluation dataset contains 15 tests"):
    dataset.tests.length shouldBe 15

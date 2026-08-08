package monad_core.simulator.infrastructure.ai.agent_evaluator.dataset

import monad_core.simulator.application.ai.AgentEvaluationDataset
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HardcodedAgentEvaluationDatasetTest extends AnyFunSuite with Matchers:

  test("hardcoded agent evaluation dataset is initially empty"):
    val dataset: AgentEvaluationDataset = HardcodedAgentEvaluationDataset

    dataset.tests shouldBe Seq.empty

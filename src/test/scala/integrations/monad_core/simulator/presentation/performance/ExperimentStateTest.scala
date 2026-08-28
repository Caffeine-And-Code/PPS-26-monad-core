package integrations.monad_core.simulator.presentation.performance

import monad_core.performance.domain.InvalidEntityCount
import monad_core.simulator.presentation.performance.{ExperimentActions, ExperimentState, isRunning}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ExperimentStateTest extends AnyFunSuite with Matchers:

  private val Report = "performance report"
  private val Error  = InvalidEntityCount(0)

  test("the initial state is ready"):
    ExperimentState.initial shouldBe ExperimentState.Ready

  test("starting a ready performance test changes its state to running"):
    val result = ExperimentActions.onStart(ExperimentState.Ready)

    result shouldBe ExperimentState.Running

  test("starting an already running performance test preserves its state"):
    val result = ExperimentActions.onStart(ExperimentState.Running)

    result shouldBe ExperimentState.Running

  test("a successful result completes a running performance test"):
    val result =
      ExperimentActions.onComplete(ExperimentState.Running, Right(Report))

    result shouldBe ExperimentState.Succeeded(Report)

  test("a domain error fails a running performance test"):
    val result =
      ExperimentActions.onComplete(ExperimentState.Running, Left(Error))

    result shouldBe ExperimentState.Failed(Error.message)

  test("an unexpected error fails a running performance test"):
    val errorMessage = "unexpected failure"

    val result =
      ExperimentActions.onFailure(ExperimentState.Running, errorMessage)

    result shouldBe ExperimentState.Failed(errorMessage)

  test("an unexpected error received outside a running state is ignored"):
    val result =
      ExperimentActions.onFailure(ExperimentState.Ready, "unexpected failure")

    result shouldBe ExperimentState.Ready

  test("a result received outside a running state is ignored"):
    val result =
      ExperimentActions.onComplete(ExperimentState.Ready, Right(Report))

    result shouldBe ExperimentState.Ready

  test("only the running performance-test state reports that it is running"):
    ExperimentState.Running.isRunning shouldBe true
    ExperimentState.Ready.isRunning shouldBe false

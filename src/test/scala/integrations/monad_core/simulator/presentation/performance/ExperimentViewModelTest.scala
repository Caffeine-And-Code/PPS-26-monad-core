package integrations.monad_core.simulator.presentation.performance

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.domain.{InvalidEntityCount, PerformanceError}
import monad_core.simulator.application.performance.{ExperimentExecutor, ExperimentRequest}
import monad_core.simulator.domain.performance.MissingPerformanceArgument
import monad_core.simulator.presentation.performance.{
  ExperimentFormArguments,
  ExperimentState,
  ExperimentViewModel
}
import monad_core.performance.presentation.{PerformanceArguments, PerformanceRoutes}
import org.scalamock.scalatest.MockFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{Future, Promise}

class ExperimentViewModelTest extends AnyFunSuite with Matchers with MockFactory:

  private val FormValues = Map(
    ExperimentFormArguments.PerformanceExperimentType -> "Load",
    PerformanceArguments.Entities                     -> "100"
  )

  private val Report = "performance report"

  test("submitting the form starts one selected run with the current physics snapshot"):
    val pendingResult                              = Promise[Either[PerformanceError, String]]()
    val physics                                    = PhysicsManager.default()
    var receivedRequest: Option[ExperimentRequest] = None
    var receivedPhysics: Option[PhysicsManager]    = None
    val runner: ExperimentExecutor[PhysicsManager] = (request, manager) =>
      receivedRequest = Some(request)
      receivedPhysics = Some(manager)
      pendingResult.future
    val viewModel = ExperimentViewModel(runner, () => physics, action => action())

    viewModel.onSubmit(FormValues)

    viewModel.state.value shouldBe ExperimentState.Running
    receivedRequest shouldBe Some(
      ExperimentRequest(
        PerformanceRoutes.Load,
        Vector(PerformanceArguments.Entities, "100")
      )
    )

  test("an invalid form fails without starting a performance run"):
    val runner: ExperimentExecutor[PhysicsManager] =
      mockFunction[
        ExperimentRequest,
        PhysicsManager,
        Future[Either[PerformanceError, String]]
      ]
    val viewModel = ExperimentViewModel(
      runner,
      () => PhysicsManager.default(),
      action => action()
    )

    viewModel.onSubmit(Map.empty)

    viewModel.state.value shouldBe ExperimentState.Failed(
      MissingPerformanceArgument(ExperimentFormArguments.PerformanceExperimentType).message
    )

  test("a successful run is published using the UI thread"):
    val pendingResult = Promise[Either[PerformanceError, String]]()
    val uiExecutor    = mockFunction[() => Unit, Unit]
    val viewModel = ExperimentViewModel(
      (_, _) => pendingResult.future,
      () => PhysicsManager.default(),
      uiExecutor
    )

    uiExecutor.expects(*).onCall((action: () => Unit) => action()).once()

    viewModel.onSubmit(FormValues)
    pendingResult.success(Right(Report))

    viewModel.state.value shouldBe ExperimentState.Succeeded(Report)

  test("a performance error changes the state to failed"):
    val error = InvalidEntityCount(0)
    val viewModel = ExperimentViewModel(
      (_, _) => Future.successful(Left(error)),
      () => PhysicsManager.default(),
      action => action()
    )

    viewModel.onSubmit(FormValues)

    viewModel.state.value shouldBe ExperimentState.Failed(error.message)

  test("an unexpected runner failure changes the state to failed"):
    val errorMessage = "runner failure"
    val viewModel = ExperimentViewModel(
      (_, _) => Future.failed(new RuntimeException(errorMessage)),
      () => PhysicsManager.default(),
      action => action()
    )

    viewModel.onSubmit(FormValues)

    viewModel.state.value shouldBe ExperimentState.Failed(errorMessage)

  test("an unexpected failure without a message uses its type as the error message"):
    val viewModel = ExperimentViewModel(
      (_, _) => Future.failed(new RuntimeException()),
      () => PhysicsManager.default(),
      action => action()
    )

    viewModel.onSubmit(FormValues)

    viewModel.state.value shouldBe ExperimentState.Failed("RuntimeException")

  test("submitting while a performance test is running does not start a second run"):
    val pendingResult = Promise[Either[PerformanceError, String]]()
    var runCount      = 0
    val runner: ExperimentExecutor[PhysicsManager] = (_, _) =>
      runCount += 1
      pendingResult.future
    val viewModel = ExperimentViewModel(
      runner,
      () => PhysicsManager.default(),
      action => action()
    )

    viewModel.onSubmit(FormValues)
    viewModel.onSubmit(FormValues)

    runCount shouldBe 1

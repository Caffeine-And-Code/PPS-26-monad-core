package monad_core.simulator.presentation.performance

import monad_core.engine.physics.core.PhysicsManager
import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.performance.{
  ExperimentDialog,
  PerformanceGameEngineModePanel
}
import monad_core.simulator.presentation.routes.RouteResponse

import scala.concurrent.{ExecutionContext, Future}

/** Connects the optional performance interface to the engine and command-line routes. */
object PerformanceMode:
  
  private val PerformanceOption = "--performance"

  /**
   * Selects the standard panel or its performance-enabled variant.
   *
   * @param arguments application command-line arguments
   * @param currentPhysics provider of the currently enabled physics rules
   * @return the panel selected by the `--performance` option
   */
  def panelFor(
      arguments: Array[String],
      currentPhysics: () => PhysicsManager
  ): GameEngineModePanelBuilder =

    if arguments.contains(PerformanceOption) then
      PerformanceGameEngineModePanel.withExperiment(
        GameEngineModePanel,
        runExperiment(currentPhysics)
      )
    else GameEngineModePanel

  /**
   * Executes a command-line performance route with the default physics configuration.
   *
   * @param arguments command-line arguments
   * @param route selected performance route
   * @return routing response containing the execution outcome
   */
  def runCommand(arguments: Array[String], route: String): RouteResponse =
    PerformanceCli.run(route, arguments, PhysicsManager.default()) match
      case Left(error) => RouteResponse(success = false, message = error.message)
      case Right(report) =>
        Console.println(report)
        RouteResponse(
          success = true,
          message = "Finished performance experiment"
        )

  private def runExperiment(
      currentPhysics: () => PhysicsManager
  ): ExperimentDialog.RunExperiment = command =>
    val physicsManager = currentPhysics()
    Future {
      PerformanceCli.run(command.route, command.arguments.toArray, physicsManager)
    }(ExecutionContext.global)

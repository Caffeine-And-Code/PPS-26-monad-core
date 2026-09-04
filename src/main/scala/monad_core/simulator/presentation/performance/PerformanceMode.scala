package monad_core.simulator.presentation.performance

import monad_core.performance.simulator.PerformanceCli
import monad_core.simulator.presentation.panels.GameEngineModePanel
import monad_core.simulator.presentation.panels.traits.GameEngineModePanelBuilder
import monad_core.simulator.presentation.routes.RouteResponse

/**
 * Selects the optional performance interface and adapts its command-line results.
 *
 * The selected panel manages its own dialog and obtains the current rule state from the
 * runtime already supplied during panel construction.
 */
object PerformanceMode:
  private val PerformanceOption = "--performance"

  /**
   * Selects the standard panel or its performance-enabled variant.
   *
   * @param arguments
   *  application command-line arguments
   * @return
   *  the panel selected by the `--performance` option
   */
  def panelFor(arguments: Array[String]): GameEngineModePanelBuilder =
    if arguments.contains(PerformanceOption) then
      PerformanceGameEngineModePanel(GameEngineModePanel)
    else GameEngineModePanel

  /**
   * Adapts a command-line performance result to the application router.
   *
   * @param arguments
   *  command-line arguments
   * @param route
   *  selected performance route
   * @return
   *  routing response containing the execution outcome
   * @see [[monad_core.simulator.presentation.routes.RouteResponse RouteResponse]]
   */
  def runCommand(arguments: Array[String], route: String): RouteResponse =
    PerformanceCli.run(route, arguments) match
      case Left(error) => RouteResponse(success = false, message = error.message)
      case Right(report) =>
        Console.println(report)
        RouteResponse(
          success = true,
          message = "Finished performance experiment"
        )

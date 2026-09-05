package monad_core.simulator.presentation.resources

/**
 * Trait representing how an image resource is viewed by the system.
 */
trait Image(
    val fileName: String,
    val width: Double,
    val height: Double,
    val preserveRatio: Boolean = true,
    val smooth: Boolean = true
)

/**
 * Image Singleton Object which provides all the static attributes of the resources usable by the Gui Application.
 *
 * @see [[ImageLoader]]
 */
object Image:

  private val iconSize: Double     = 32.0
  private val menuToolSize: Double = 28.0

  case class PlayIcon()
      extends Image(
        fileName = "play.png",
        width = iconSize,
        height = iconSize
      )

  case class PauseIcon()
      extends Image(
        fileName = "pause.png",
        width = iconSize,
        height = iconSize
      )

  case class StopIcon()
      extends Image(
        fileName = "stop.png",
        width = iconSize,
        height = iconSize
      )

  case class ToolsIcon()
      extends Image(
        fileName = "tools.png",
        width = menuToolSize,
        height = menuToolSize
      )

  case class PhysicsIcon()
      extends Image(
        fileName = "physics.png",
        width = menuToolSize,
        height = menuToolSize
      )

  case class PerformanceIcon()
      extends Image(
        fileName = "performance.png",
        width = menuToolSize,
        height = menuToolSize
      )

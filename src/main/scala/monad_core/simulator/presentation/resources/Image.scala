package monad_core.simulator.presentation.resources

trait Image(
    val fileName: String,
    val width: Double,
    val height: Double,
    val preserveRatio: Boolean = true,
    val smooth: Boolean = true
)

object Image:

  private val iconSize: Double = 32.0

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
        width = 28,
        height = 28
      )

  case class PhysicsIcon()
      extends Image(
        fileName = "physics.png",
        width = 28,
        height = 28
      )

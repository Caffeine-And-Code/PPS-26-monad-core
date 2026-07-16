package graphics.resources

import graphics.stages.support.Size

trait Image(
                    val fileName: String,
                    val width: Double,
                    val height: Double,
                    val preserveRatio: Boolean = true,
                    val smooth: Boolean = true
                  )

object Image:

  private val iconSize : Size[Double] = Size(32.0, 32.0)

  case class PlayIcon() extends Image(
    fileName = "play.png",
    width = iconSize.width,
    height = iconSize.height
  )

  case class PauseIcon() extends Image(
    fileName = "pause.png",
    width = iconSize.width,
    height = iconSize.height
  )

  case class StopIcon() extends Image(
    fileName = "stop.png",
    width = iconSize.width,
    height = iconSize.height
  )
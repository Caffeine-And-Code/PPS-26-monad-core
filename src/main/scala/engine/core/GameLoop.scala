package engine.core

sealed trait EngineMode
case object EditMode extends EngineMode
case object SimulationMode extends EngineMode

val defaultTickTime = 0.016

case class GameLoop(
                     mode: EngineMode = EditMode,
                     tickTime: Double = defaultTickTime,
                     isRunning: Boolean = false
                   ):

  def withMode(newMode: EngineMode): GameLoop =
    this.copy(mode = newMode)

  def withTickTime(newTickTime: Double): GameLoop =
    this.copy(tickTime = newTickTime)

  def start(): GameLoop = this.copy(isRunning = true)
  def stop(): GameLoop = this.copy(isRunning = false)

end GameLoop
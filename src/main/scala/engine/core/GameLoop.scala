package engine.core

import engine.core.traits.{PhysicsEngine, RenderEngine, Scene}

import scala.annotation.tailrec

sealed trait EngineMode
case object EditMode extends EngineMode
case object SimulationMode extends EngineMode

val defaultTickTime = 16_000_000L
val initialTime = 0L
val initialAccumulatorValue = 0L
val defaultMaxFrameTime = 250_000_000L
val staticAlpha = 1.0

case class GameLoop(
                     mode: EngineMode = EditMode,
                     tickTime: Long = defaultTickTime,
                     isRunning: Boolean = false,
                     lastTime: Long = initialTime,
                     accumulator: Long = initialAccumulatorValue,
                     maxFrameTime: Long = defaultMaxFrameTime
                   ):

  def withMode(newMode: EngineMode): GameLoop = {
    this.copy(mode = newMode)  
  }

  def withTickTime(newTickTime: Long): GameLoop =
    this.copy(tickTime = newTickTime)

  def start(): GameLoop =
    this.copy(isRunning = true)

  def stop(): GameLoop =
    this.copy(isRunning = false)

  def tick(scene: Scene, physicsEngine: PhysicsEngine, renderEngine:RenderEngine, currentTime: Long): (Scene, GameLoop) =
    if !isRunning || mode == EditMode then {
      renderEngine.render(scene, staticAlpha)
      (scene, this.copy(lastTime = currentTime))
    } else
      val elapsedTime = currentTime - lastTime
      val clampedTime = Math.min(elapsedTime, maxFrameTime)
      val remainingTime = this.accumulator + clampedTime

      @tailrec
      def runFixedUpdate(remainingTime: Long, currentScene: Scene): (Scene, Long) =
        if remainingTime < tickTime then
          (currentScene, remainingTime)
        else
          val updatedScene = physicsEngine.step(currentScene, tickTime)
          runFixedUpdate(remainingTime - tickTime, updatedScene)

      val (currentScene, currentAccumulator) = runFixedUpdate(remainingTime, scene)

      val alpha = currentAccumulator.toDouble / tickTime.toDouble
      renderEngine.render(currentScene, alpha)

      (currentScene, this.copy(lastTime = currentTime, accumulator = currentAccumulator))


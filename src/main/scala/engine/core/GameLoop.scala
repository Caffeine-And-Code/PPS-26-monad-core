package engine.core

import engine.core.traits.{Physics, RenderEngine, Scene}

import scala.annotation.tailrec

sealed trait EngineMode
case object EditMode extends EngineMode
case object SimulationMode extends EngineMode

val DefaultTickTime = 16_000_000L
val InitialTime = 0L
val InitialAccumulatorValue = 0L
val DefaultMaxFrameTime = 250_000_000L
val StaticAlpha = 1.0

case class GameLoop(
                     mode: EngineMode = EditMode,
                     tickTime: Long = DefaultTickTime,
                     isRunning: Boolean = false,
                     lastTime: Long = InitialTime,
                     accumulator: Long = InitialAccumulatorValue,
                     maxFrameTime: Long = DefaultMaxFrameTime
                   )

object GameLoop:
  extension (gameLoop: GameLoop)
    def withMode(newMode: EngineMode): GameLoop =
      gameLoop.copy(mode = newMode)

    def withTickTime(newTickTime: Long): GameLoop =
      gameLoop.copy(tickTime = newTickTime)

    def start(): GameLoop =
      gameLoop.copy(isRunning = true)

    def stop(): GameLoop =
      gameLoop.copy(isRunning = false)

    def tick(scene: Scene, physicsEngine: Physics, renderEngine:RenderEngine, currentTime: Long): (Scene, GameLoop) =
      if !gameLoop.isRunning || gameLoop.mode == EditMode then
        renderEngine.render(scene, StaticAlpha)
        (scene, gameLoop.copy(lastTime = currentTime))
      else
        val elapsedTime = currentTime - gameLoop.lastTime
        val clampedTime = Math.min(elapsedTime, gameLoop.maxFrameTime)
        val remainingTime = gameLoop.accumulator + clampedTime

        @tailrec
        def runFixedUpdate(remainingTime: Long, currentScene: Scene): (Scene, Long) =
          if remainingTime < gameLoop.tickTime then
            (currentScene, remainingTime)
          else
            val updatedScene = physicsEngine.step(currentScene, gameLoop.tickTime)
            runFixedUpdate(remainingTime - gameLoop.tickTime, updatedScene)

        val (currentScene, currentAccumulator) = runFixedUpdate(remainingTime, scene)

        val alpha = currentAccumulator.toDouble / gameLoop.tickTime.toDouble
        renderEngine.render(currentScene, alpha)

        (currentScene, gameLoop.copy(lastTime = currentTime, accumulator = currentAccumulator))


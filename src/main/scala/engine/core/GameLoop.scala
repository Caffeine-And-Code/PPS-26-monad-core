package engine.core

import engine.core.traits.{PhysicsEngine, RenderEngine, State}

import scala.annotation.tailrec

val DefaultTickTime = 16_000_000L
val InitialTime = 0L
val InitialAccumulatorValue = 0L
val DefaultMaxFrameTime = 250_000_000L
val StaticAlpha = 1.0

case class GameLoop(
                     mode: LoopMode = EditMode,
                     tickTime: Long = DefaultTickTime,
                     isRunning: Boolean = false,
                     lastTime: Long = InitialTime,
                     accumulator: Long = InitialAccumulatorValue,
                     maxFrameTime: Long = DefaultMaxFrameTime
                   ):
  require(tickTime > 0, "tick time cannot be negative or zero")
  require(lastTime >= 0, "last time cannot be negative")
  require(accumulator >= 0, "accumulator cannot be negative")
  require(maxFrameTime > 0, "max frame time cannot be negative or zero")
  require(maxFrameTime >= tickTime, "max frame time cannot be less than tick time")

object GameLoop:
  extension (gameLoop: GameLoop)
    def withMode(newMode: LoopMode): GameLoop =
      gameLoop.copy(mode = newMode)

    def withTickTime(newTickTime: Long): GameLoop =
      gameLoop.copy(tickTime = newTickTime)

    def start(): GameLoop =
      gameLoop.copy(isRunning = true)

    def stop(): GameLoop =
      gameLoop.copy(isRunning = false)

    def tick(state: Scene, physicsEngine: PhysicsEngine, renderEngine: RenderEngine, currentTime: Long): (Scene, GameLoop) =
      if !gameLoop.isRunning || gameLoop.mode == EditMode then
        renderEngine.render(state, StaticAlpha)
        (state, gameLoop.copy(lastTime = currentTime))
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

        val (currentScene, currentAccumulator) = runFixedUpdate(remainingTime, state)

        val alpha = currentAccumulator.toDouble / gameLoop.tickTime.toDouble
        renderEngine.render(currentScene, alpha)

        (currentScene, gameLoop.copy(lastTime = currentTime, accumulator = currentAccumulator))


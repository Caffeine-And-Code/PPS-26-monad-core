package monad_core.engine.core

import monad_core.engine.core.traits.{PhysicsEngine, State}
import monad_core.engine.model.EngineError
import monad_core.engine.simulator.Painter

import scala.annotation.tailrec

private case class GameLoopImpl(
    mode: LoopMode,
    tickTime: Long,
    isRunning: Boolean,
    lastTime: Long,
    accumulator: Long,
    maxFrameTime: Long
) extends GameLoop:

  def withMode(newMode: LoopMode): GameLoop = this.copy(mode = newMode)

  def withTickTime(newTickTime: Long): Either[EngineError, GameLoop] =
    if newTickTime <= 0 then Left(InvalidTickTime(newTickTime))
    else if maxFrameTime < newTickTime then
      Left(InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, newTickTime))
    else Right(this.copy(tickTime = newTickTime))

  def start(): GameLoop = this.copy(isRunning = true, mode = LoopMode.SimulationMode)
  def stop(): GameLoop  = this.copy(isRunning = false, mode = LoopMode.EditMode)

  def tick(state: State, currentTime: Long)(using
      physics: PhysicsEngine,
      painter: Painter
  ): Either[EngineError, (State, GameLoop)] =
    if !isRunning || mode == LoopMode.EditMode then
      RendererManager.render(state)
      Right((state, this.copy(lastTime = currentTime)))
    else
      val elapsedTime   = currentTime - lastTime
      val clampedTime   = Math.min(elapsedTime, maxFrameTime)
      val remainingTime = accumulator + clampedTime

      for
        res <- runFixedUpdate(remainingTime, state, state)
        (previousScene, currentScene, currentAccumulator) = res
        alpha                              = currentAccumulator.toDouble / tickTime.toDouble
        interpolatedScene <- SceneInterpolator(
          previousScene = previousScene,
          nextScene = currentScene,
          interpolationAlpha = alpha
        )
      yield
        RendererManager.render(interpolatedScene)
        (currentScene, this.copy(lastTime = currentTime, accumulator = currentAccumulator))

  @tailrec
  private def runFixedUpdate(
                              remainingTime: Long,
                              previousScene: State,
                              currentScene: State
                            )
                            (using physics: PhysicsEngine)
  : Either[EngineError, (State, State, Long)] =
    if remainingTime < tickTime then Right((previousScene, currentScene, remainingTime))
    else
      val updatedScene = physics.step(currentScene, tickTime)
      updatedScene match
        case Left(err)           => Left(err)
        case Right(updatedScene) =>
          runFixedUpdate(remainingTime - tickTime, currentScene, updatedScene)


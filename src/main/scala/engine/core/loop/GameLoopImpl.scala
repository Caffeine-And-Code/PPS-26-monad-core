package engine.core.loop

import engine.core.loop.GameLoop.StaticAlpha
import engine.core.traits.RenderEngine
import engine.errors.EngineError
import engine.physics.Physics

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
    else if maxFrameTime < newTickTime then Left(InvalidMaxFrameTimeTickTimeRatio(maxFrameTime, newTickTime))
    else Right(this.copy(tickTime = newTickTime))

  def start(): GameLoop = this.copy(isRunning = true)
  def stop(): GameLoop = this.copy(isRunning = false)

  def tick[S](scene: S, currentTime: Long)(using physics: Physics[S], render: RenderEngine[S]): Either[EngineError, (S, GameLoop)] =
    if !isRunning || mode == LoopMode.EditMode then
      render.render(scene, StaticAlpha)
      Right((scene, this.copy(lastTime = currentTime)))
    else
      val elapsedTime = currentTime - lastTime
      val clampedTime = Math.min(elapsedTime, maxFrameTime)
      val remainingTime = accumulator + clampedTime

      @tailrec
      def runFixedUpdate(remainingTime: Long, currentScene: S): Either[EngineError, (S, Long)] =
        if remainingTime < tickTime then
          Right((currentScene, remainingTime))
        else
          physics.step(currentScene, tickTime) match
            case Left(error) => Left(error)
            case Right(updatedScene) => runFixedUpdate(remainingTime - tickTime, updatedScene)

      for
        res <- runFixedUpdate(remainingTime, scene)
        (currentScene, currentAccumulator) = res
        _ = {
          val alpha = currentAccumulator.toDouble / tickTime.toDouble
          render.render(currentScene, alpha)
        }
      yield (currentScene, this.copy(lastTime = currentTime, accumulator = currentAccumulator))

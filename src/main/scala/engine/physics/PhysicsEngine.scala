package engine.physics

import engine.core.Scene
import engine.model.{Vector2D, add, times}

object PhysicsEngine:

  private val DefaultForces = Vector2D(0, 0)
  private val DefaultSpeed = Vector2D(0, 0)
  private val DefaultMass = 1.0
  private val NanoInSeconds = 1_000_000_000.0

  def step(scene: Scene, deltaTime: Long): Either[PhysicsError, Scene] =

    if deltaTime < 0 then
      Left(NegativeDeltaTime(deltaTime))

    else if deltaTime == 0L then
      Right(scene)

    else
      Right(scene.copy())
package engine.physics

import engine.core.Scene
import engine.model.{Vector2D, add, times}

case class PhysicsEngine()

object PhysicsEngine :

  private val DefaultForces = Vector2D(0, 0)
  private val DefaultSpeed = Vector2D(0, 0)
  private val DefaultMass = 1.0
  private val NanoInSeconds = 1_000_000_000.0

  extension (physicsEngine: PhysicsEngine)
    def step(scene: Scene, dt: Long): Scene =
      require(dt >= 0, "Time difference cannot be negative")
      if dt == 0L then return scene

      val dtSeconds = dt / NanoInSeconds

      val surfaceForce = scene.surfaces.values.headOption
        .flatMap(_.appliedForce)
        .getOrElse(DefaultForces)

      val updatedEntities = scene.entities.map : (id, entity) =>
        if (entity.isFixed) then
          id -> entity
        else 
          val mass = entity.weight.map(_.value.toDouble).getOrElse(DefaultMass)
          val acceleration = surfaceForce times (1.0 / mass)

          val currentSpeed = entity.speed.getOrElse(DefaultSpeed)
          val newSpeed = currentSpeed add (acceleration times dtSeconds)
          val displacement = newSpeed times dtSeconds

          val updatedEntity = entity.withSpeed(newSpeed)
            .flatMap(_.moveBy(displacement))
            .getOrElse(entity)

          id -> updatedEntity
          
      scene.copy(entities = updatedEntities)
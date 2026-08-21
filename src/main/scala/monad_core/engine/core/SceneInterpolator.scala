package monad_core.engine.core

import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.physics.utils.Rotation

object SceneInterpolator:

  def apply(
      previousScene: State,
      nextScene: State,
      interpolationAlpha: Double
  ): Either[EngineError, Scene] =
    for
      alpha  <- validateAlpha(interpolationAlpha)
      bounds <- interpolateBounds(previousScene.bounds, nextScene.bounds, alpha)
      entities <- interpolateEntities(
        previousScene.allEntities,
        nextScene.allEntities,
        alpha
      )
    yield Scene(
      entities = entities,
      surfaces = nextScene.allSurfaces.map(surface => surface.id -> surface).toMap,
      teams = nextScene.allTeams.map(team => team.id -> team).toMap,
      bounds = bounds
    )

  private def validateAlpha(alpha: Double): Either[EngineError, Double] =
    if alpha < 0.0 || alpha > 1.0 then Left(InvalidInterpolationAlpha(alpha))
    else Right(alpha)

  private def interpolateBounds(
      previousBounds: WorldBounds,
      nextBounds: WorldBounds,
      alpha: Double
  ): Either[EngineError, WorldBounds] =
    val lowerRight = interpolateVector(
      previousBounds.lowerRight,
      nextBounds.lowerRight,
      alpha
    )

    WorldBounds(lowerRight.x, lowerRight.y)

  private def interpolateEntities(
      previousEntities: List[Entity],
      nextEntities: List[Entity],
      alpha: Double
  ): Either[EngineError, EntityMap] =
    val previousById = previousEntities.map(entity => entity.id -> entity).toMap

    nextEntities
      .foldLeft(Right(Map.empty): Either[EngineError, EntityMap]): (result, nextEntity) =>
        for
          entities <- result
          interpolatedEntity <- previousById.get(nextEntity.id) match
            case Some(previousEntity) =>
              nextEntity
                .moveTo(
                  interpolateVector(
                    previousEntity.position,
                    nextEntity.position,
                    alpha
                  )
                )
                .rotateTo(
                  Rotation.interpolate(
                    previousEntity.rotation,
                    nextEntity.rotation,
                    alpha
                  )
                )
            case None => Right(nextEntity)
        yield entities.updated(interpolatedEntity.id, interpolatedEntity)

  private def interpolateVector(
      previous: Vector2D,
      next: Vector2D,
      alpha: Double
  ): Vector2D =
    previous + (next - previous) * alpha

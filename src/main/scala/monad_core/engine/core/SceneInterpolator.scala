package monad_core.engine.core

import monad_core.engine.core.traits.State
import monad_core.engine.model.*

object SceneInterpolator:

  def apply(
      previousScene: State,
      nextScene: State,
      interpolationAlpha: Double
  ): Either[EngineError, Scene] =
    for
      alpha  <- validateAlpha(interpolationAlpha)
      bounds <- interpolateBounds(previousScene.bounds, nextScene.bounds, alpha)
    yield Scene(
      entities = interpolateEntities(
        previousScene.allEntities,
        nextScene.allEntities,
        alpha
      ),
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
  ): EntityMap =
    val previousById = previousEntities.map(entity => entity.id -> entity).toMap

    nextEntities.map { nextEntity =>
      val interpolatedEntity = previousById
        .get(nextEntity.id)
        .map(previousEntity =>
          nextEntity.moveTo(
            interpolateVector(
              previousEntity.position,
              nextEntity.position,
              alpha
            )
          )
        )
        .getOrElse(nextEntity)

      interpolatedEntity.id -> interpolatedEntity
    }.toMap

  private def interpolateVector(
      previous: Vector2D,
      next: Vector2D,
      alpha: Double
  ): Vector2D =
    previous + (next - previous) * alpha

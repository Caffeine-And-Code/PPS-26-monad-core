package monad_core.engine.simulator

import monad_core.engine.core.InvalidInterpolationAlpha
import monad_core.engine.core.traits.State
import monad_core.engine.model.*
import monad_core.engine.physics.utils.Rotation

/** Produces renderable intermediate scenes between two simulation states. */
object StateInterpolator:

  /**
   * Interpolates bounds and matching entities while retaining the next state's static data.
   *
   * @param previousScene
   *   state before the latest physics tick
   * @param nextScene
   *   state produced by the latest physics tick
   * @param interpolationAlpha
   *   interpolation ratio between zero and one
   * @return
   *   interpolated scene, or a [[EngineError]]
   */
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

  /**
   * Validates an interpolation ratio.
   *
   * @param alpha
   *   candidate ratio
   * @return
   *   ratio between zero and one, or a [[EngineError]]
   */
  private def validateAlpha(alpha: Double): Either[EngineError, Double] =
    if alpha < 0.0 || alpha > 1.0 then Left(InvalidInterpolationAlpha(alpha))
    else Right(alpha)

  /**
   * Interpolates world dimensions and validates the resulting bounds.
   *
   * @param previousBounds
   *   starting world bounds
   * @param nextBounds
   *   target world bounds
   * @param alpha
   *   interpolation ratio
   * @return
   *   interpolated bounds, or a [[EngineError]]
   */
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

  /**
   * Interpolates entities present in both states and retains newly added entities.
   *
   * @param previousEntities
   *   entities before the latest physics tick
   * @param nextEntities
   *   entities produced by the latest physics tick
   * @param alpha
   *   interpolation ratio
   * @return
   *   interpolated entities indexed by identifier, or a [[EngineError]]
   */
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

  /**
   * Linearly interpolates between two vectors by the interpolation ratio.
   *
   * @param previous
   *   starting vector
   * @param next
   *   target vector
   * @param alpha
   *   interpolation ratio
   * @return
   *   interpolated vector
   */
  private def interpolateVector(
      previous: Vector2D,
      next: Vector2D,
      alpha: Double
  ): Vector2D =
    previous + (next - previous) * alpha

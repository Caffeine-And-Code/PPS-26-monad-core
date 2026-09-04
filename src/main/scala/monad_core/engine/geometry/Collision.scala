package monad_core.engine.geometry

import monad_core.engine.model.Vector2D

/**
 * Geometric information produced when two placed shapes collide.
 *
 * @param normalVector unit vector oriented from the first shape toward the second shape
 * @param penetrationDepth non-negative overlap measured along `normalVector`; zero represents contact without overlap
 * @param collisionPoint contact point
 */
final case class Collision(
    normalVector: Vector2D,
    penetrationDepth: Double,
    collisionPoint: Vector2D
)

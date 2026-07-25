package monad_core.engine.geometry

import monad_core.engine.model.Vector2D

final case class Collision(normalVector: Vector2D, penetrationDepth: Double)
package engine.geometry

import engine.model.Vector2D

final case class Collision(normalVector: Vector2D, penetrationDepth: Double)
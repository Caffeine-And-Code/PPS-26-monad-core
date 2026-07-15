package engine.geometry

import engine.model.Vector2D

final case class Collision(normal: Vector2D, penetrationDepth: Double)
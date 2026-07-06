package engine.geometry

import engine.model.Vector2D

final case class Placed[+A](center: Vector2D, value: A)

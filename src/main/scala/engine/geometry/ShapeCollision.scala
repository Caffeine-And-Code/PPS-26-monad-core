package engine.geometry

import engine.model.Shape2D.Circle
import engine.model.*

object ShapeCollision:

  given circleCollidesWithCircle: Collides[Circle, Circle] with

    override def collision(first: Placed[Circle], second: Placed[Circle]): Option[Collision] =
      val distance = first.center --> second.center
      val penetrationDepth = first.value.radius + second.value.radius - distance

      Option.when(penetrationDepth >= 0):
        Collision(calculateNorm(first.center, second.center), penetrationDepth)


  given shapeCollidesWIthShape: Collides[Shape2D, Shape2D] with

    override def collision(first: Placed[Shape2D], second: Placed[Shape2D]): Option[Collision] =
      (first.value, second.value) match {
        case (firstCircle: Circle, secondCircle: Circle) =>
          circleCollidesWithCircle.collision(Placed(first.center, firstCircle), Placed(second.center, secondCircle))
        case (_, _) => ???
      }


  private def calculateNorm(firstPoint: Vector2D, secondPoint: Vector2D): Vector2D =
    (secondPoint - firstPoint).normalized
package engine.geometry

import engine.model.*
import engine.model.Shape2D.{Circle, Rectangle}

object ShapeCollision:

  given circleCollidesWithCircle: Collides[Circle, Circle] with

    override def collision(first: Placed[Circle], second: Placed[Circle]): Option[Collision] =
      val distance = first.center --> second.center
      val penetrationDepth = first.value.radius + second.value.radius - distance

      Option.when(penetrationDepth >= 0):
        Collision(calculateNorm(first.center, second.center), penetrationDepth)

  given rectangleCollidesWithRectangle: Collides[Rectangle, Rectangle] with

    override def collision(first: Placed[Rectangle], second: Placed[Rectangle]): Option[Collision] =
      val halfWidthSum = halfLength(first.value) + halfLength(second.value)
      val halfHeightSum = halfHeight(first.value) + halfHeight(second.value)
      val distanceX = second.center.x - first.center.x
      val distanceY = second.center.y - first.center.y
      val overlapX = halfWidthSum - math.abs(distanceX)
      val overlapY = halfHeightSum - math.abs(distanceY)

      Option.when(overlapX >= 0 && overlapY >= 0):
        if overlapX <= overlapY then
          Collision(Vector2D(sign(distanceX), 0), overlapX)
        else
          Collision(Vector2D(0, sign(distanceY)), overlapY)

  given shapeCollidesWIthShape: Collides[Shape2D, Shape2D] with

    override def collision(first: Placed[Shape2D], second: Placed[Shape2D]): Option[Collision] =
      (first.value, second.value) match {
        case (firstCircle: Circle, secondCircle: Circle) =>
          circleCollidesWithCircle.collision(Placed(first.center, firstCircle), Placed(second.center, secondCircle))
        case (firstRectangle: Rectangle, secondRectangle: Rectangle) =>
          rectangleCollidesWithRectangle.collision(Placed(first.center, firstRectangle), Placed(second.center, secondRectangle))
        case (_, _) => ???
      }

  private def calculateNorm(firstPoint: Vector2D, secondPoint: Vector2D): Vector2D =
    (secondPoint - firstPoint).normalized

  private def halfLength(rectangle: Rectangle): Double =
    rectangle.length / 2

  private def halfHeight(rectangle: Rectangle): Double =
    rectangle.height / 2

  private def sign(value: Double): Double =
    if value < 0 then -1 else 1
package engine.geometry

import engine.model.*
import engine.model.Shape2D.{Circle, Rectangle}

object ShapeCollision:

  private def collisionFromCircleInsideRectangle(circle: Placed[Circle], rectangle: Placed[Rectangle]): Collision =
    val distanceToRightEdge = rectangle.center.x + halfLength(rectangle.value) - circle.center.x
    val distanceToLeftEdge = circle.center.x - (rectangle.center.x - halfLength(rectangle.value))
    val distanceToTopEdge = rectangle.center.y + halfHeight(rectangle.value) - circle.center.y
    val distanceToBottomEdge = circle.center.y - (rectangle.center.y - halfHeight(rectangle.value))

    val nearestEdge = Seq(
      (distanceToRightEdge, Vector2D(1, 0)),
      (distanceToLeftEdge, Vector2D(-1, 0)),
      (distanceToTopEdge, Vector2D(0, 1)),
      (distanceToBottomEdge, Vector2D(0, -1))
    ).minBy(_._1)

    Collision(nearestEdge._2, nearestEdge._1)

  private def halfLength(rectangle: Rectangle): Double =
    rectangle.length / 2

  private def halfHeight(rectangle: Rectangle): Double =
    rectangle.height / 2

  private def calculateNorm(firstPoint: Vector2D, secondPoint: Vector2D): Vector2D =
    (secondPoint - firstPoint).normalized

  private def sign(value: Double): Double =
    if value < 0 then -1 else 1

  private def clamp(value: Double, min: Double, max: Double): Double =
    math.max(min, math.min(max, value))

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

  given circleCollidesWithRectangle: Collides[Circle, Rectangle] with

    override def collision(circle: Placed[Circle], rectangle: Placed[Rectangle]): Option[Collision] =
      val closestPoint = Vector2D(
        clamp(circle.center.x, rectangle.center.x - halfLength(rectangle.value), rectangle.center.x + halfLength(rectangle.value)),
        clamp(circle.center.y, rectangle.center.y - halfHeight(rectangle.value), rectangle.center.y + halfHeight(rectangle.value))
      )
      val circleToClosestPoint = circle.center - closestPoint
      val distance = circleToClosestPoint.magnitude

      if distance > 0 then
        val penetrationDepth = circle.value.radius - distance
        Option.when(penetrationDepth >= 0):
          Collision(circleToClosestPoint.normalized, penetrationDepth)
      else
        Some(collisionFromCircleInsideRectangle(circle, rectangle))

  given rectangleCollidesWithCircle: Collides[Rectangle, Circle] with

    override def collision(rectangle: Placed[Rectangle], circle: Placed[Circle]): Option[Collision] =
      circleCollidesWithRectangle.collision(circle, rectangle)

  given shapeCollidesWIthShape: Collides[Shape2D, Shape2D] with

    override def collision(first: Placed[Shape2D], second: Placed[Shape2D]): Option[Collision] =
      (first.value, second.value) match {
        case (firstCircle: Circle, secondCircle: Circle) =>
          circleCollidesWithCircle.collision(Placed(first.center, firstCircle), Placed(second.center, secondCircle))
        case (firstRectangle: Rectangle, secondRectangle: Rectangle) =>
          rectangleCollidesWithRectangle.collision(Placed(first.center, firstRectangle), Placed(second.center, secondRectangle))
        case (circle: Circle, rectangle: Rectangle) =>
          circleCollidesWithRectangle.collision(Placed(first.center, circle), Placed(second.center, rectangle))
        case (rectangle: Rectangle, circle: Circle) =>
          rectangleCollidesWithCircle.collision(Placed(first.center, rectangle), Placed(second.center, circle))
      }
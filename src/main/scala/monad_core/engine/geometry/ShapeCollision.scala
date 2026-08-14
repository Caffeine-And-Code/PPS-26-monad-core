package monad_core.engine.geometry

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}

object ShapeCollision:

  private def collisionFromCircleInsideRectangle(
      circle: Placed[Circle],
      rectangle: Placed[Rectangle]
  ): Collision =
    val distanceToRightEdge  = rectangle.center.x + rectangle.shape.halfLength - circle.center.x
    val distanceToLeftEdge   = circle.center.x - (rectangle.center.x - rectangle.shape.halfLength)
    val distanceToTopEdge    = rectangle.center.y + rectangle.shape.halfHeight - circle.center.y
    val distanceToBottomEdge = circle.center.y - (rectangle.center.y - rectangle.shape.halfHeight)

    val nearestEdge = Seq(
      (distanceToRightEdge, Vector2D(-1, 0)),
      (distanceToLeftEdge, Vector2D(1, 0)),
      (distanceToTopEdge, Vector2D(0, -1)),
      (distanceToBottomEdge, Vector2D(0, 1))
    ).minBy(_._1)

    Collision(nearestEdge._2, nearestEdge._1)

  private def calculateNorm(firstPoint: Vector2D, secondPoint: Vector2D): Vector2D =
    (secondPoint - firstPoint).normalized

  /**
   * return 1 if value is positive 0 otherwise (0 is considered positive).
   */
  private def sign(value: Double): Double =
    if value < 0 then -1 else 1

  private def clamp(value: Double, min: Double, max: Double): Double =
    math.max(min, math.min(max, value))

  given circleCollidesWithCircle: Collides[Circle, Circle] with

    override def checkCollision(first: Placed[Circle], second: Placed[Circle]): Option[Collision] =
      val distance         = first.center --> second.center
      val penetrationDepth = first.shape.radius + second.shape.radius - distance

      Option.when(penetrationDepth >= 0):
        Collision(calculateNorm(first.center, second.center), penetrationDepth)

  given rectangleCollidesWithRectangle: Collides[Rectangle, Rectangle] with

    override def checkCollision(
        first: Placed[Rectangle],
        second: Placed[Rectangle]
    ): Option[Collision] =
      val halfWidthSum  = first.shape.halfLength + second.shape.halfLength
      val halfHeightSum = first.shape.halfHeight + second.shape.halfHeight
      val distanceX     = second.center.x - first.center.x
      val distanceY     = second.center.y - first.center.y
      val overlapX      = halfWidthSum - math.abs(distanceX)
      val overlapY      = halfHeightSum - math.abs(distanceY)

      Option.when(overlapX >= 0 && overlapY >= 0):
        if overlapX <= overlapY then Collision(Vector2D(sign(distanceX), 0), overlapX)
        else Collision(Vector2D(0, sign(distanceY)), overlapY)

  given circleCollidesWithRectangle: Collides[Circle, Rectangle] with

    override def checkCollision(
        circle: Placed[Circle],
        rectangle: Placed[Rectangle]
    ): Option[Collision] =
      val closestPoint = Vector2D(
        clamp(
          circle.center.x,
          rectangle.center.x - rectangle.shape.halfLength,
          rectangle.center.x + rectangle.shape.halfLength
        ),
        clamp(
          circle.center.y,
          rectangle.center.y - rectangle.shape.halfHeight,
          rectangle.center.y + rectangle.shape.halfHeight
        )
      )
      val circleToClosestPoint = closestPoint - circle.center
      val distance             = circleToClosestPoint.magnitude

      if distance > 0 then
        val penetrationDepth = circle.shape.radius - distance
        Option.when(penetrationDepth >= 0):
          Collision(circleToClosestPoint.normalized, penetrationDepth)
      else Some(collisionFromCircleInsideRectangle(circle, rectangle))

  given rectangleCollidesWithCircle: Collides[Rectangle, Circle] with

    override def checkCollision(
        rectangle: Placed[Rectangle],
        circle: Placed[Circle]
    ): Option[Collision] =
      circleCollidesWithRectangle
        .checkCollision(circle, rectangle)
        .map(collision => collision.copy(normalVector = collision.normalVector.flip))

  given shapeCollidesWithShape: Collides[Shape2D, Shape2D] with

    override def checkCollision(
        first: Placed[Shape2D],
        second: Placed[Shape2D]
    ): Option[Collision] =
      (first.shape, second.shape) match {
        case (firstCircle: Circle, secondCircle: Circle) =>
          circleCollidesWithCircle.checkCollision(
            Placed(first.center, firstCircle),
            Placed(second.center, secondCircle)
          )
        case (firstRectangle: Rectangle, secondRectangle: Rectangle) =>
          rectangleCollidesWithRectangle.checkCollision(
            Placed(first.center, firstRectangle),
            Placed(second.center, secondRectangle)
          )
        case (circle: Circle, rectangle: Rectangle) =>
          circleCollidesWithRectangle.checkCollision(
            Placed(first.center, circle),
            Placed(second.center, rectangle)
          )
        case (rectangle: Rectangle, circle: Circle) =>
          rectangleCollidesWithCircle.checkCollision(
            Placed(first.center, rectangle),
            Placed(second.center, circle)
          )
      }

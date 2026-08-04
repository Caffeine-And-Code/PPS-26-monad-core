package monad_core.engine.geometry

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}

object ShapeCollision:

  private def calculateNorm(firstPoint: Vector2D, secondPoint: Vector2D): Vector2D =
    (secondPoint - firstPoint).normalized

  private def clamp(value: Double, min: Double, max: Double): Double =
    math.max(min, math.min(max, value))

  private def axes(rectangle: Placed[Rectangle]): Seq[Vector2D] =
    Seq(Vector2D(1, 0).rotated(rectangle.rotation), Vector2D(0, 1).rotated(rectangle.rotation))

  private def projectionRadius(rectangle: Placed[Rectangle], axis: Vector2D): Double =
    val rectangleAxes = axes(rectangle)
    rectangle.shape.halfLength * math.abs(rectangleAxes.head dot axis) +
      rectangle.shape.halfHeight * math.abs(rectangleAxes.last dot axis)

  private def supportPoint(rectangle: Placed[Rectangle], direction: Vector2D): Vector2D =
    val rectangleAxes = axes(rectangle)
    val lengthDirection = if rectangleAxes.head.dot(direction) >= 0 then 1 else -1
    val heightDirection = if rectangleAxes.last.dot(direction) >= 0 then 1 else -1
    rectangle.center + rectangleAxes.head * rectangle.shape.halfLength * lengthDirection +
      rectangleAxes.last * rectangle.shape.halfHeight * heightDirection

  private def localPoint(point: Vector2D, reference: Placed[?]): Vector2D =
    (point - reference.center).rotated(-reference.rotation)

  private def worldPoint(point: Vector2D, reference: Placed[?]): Vector2D =
    reference.center + point.rotated(reference.rotation)

  private def collisionFromCircleInsideRectangle(circle: Placed[Circle], rectangle: Placed[Rectangle], localCircle: Vector2D): Collision =
    val nearestEdge = Seq(
      (rectangle.shape.halfLength - localCircle.x, Vector2D(-1, 0), Vector2D(rectangle.shape.halfLength, localCircle.y)),
      (rectangle.shape.halfLength + localCircle.x, Vector2D(1, 0), Vector2D(-rectangle.shape.halfLength, localCircle.y)),
      (rectangle.shape.halfHeight - localCircle.y, Vector2D(0, -1), Vector2D(localCircle.x, rectangle.shape.halfHeight)),
      (rectangle.shape.halfHeight + localCircle.y, Vector2D(0, 1), Vector2D(localCircle.x, -rectangle.shape.halfHeight))
    ).minBy(_._1)

    Collision(
      nearestEdge._2.rotated(rectangle.rotation),
      nearestEdge._1,
      worldPoint(nearestEdge._3, rectangle)
    )

  given circleCollidesWithCircle: Collides[Circle, Circle] with

    override def checkCollision(first: Placed[Circle], second: Placed[Circle]): Option[Collision] =
      val distance = first.center --> second.center
      val penetrationDepth = first.shape.radius + second.shape.radius - distance

      Option.when(penetrationDepth >= 0):
        val normal = calculateNorm(first.center, second.center)
        val firstContact = first.center + normal * first.shape.radius
        val secondContact = second.center - normal * second.shape.radius
        Collision(normal, penetrationDepth, (firstContact + secondContact) * 0.5)

  given rectangleCollidesWithRectangle: Collides[Rectangle, Rectangle] with

    override def checkCollision(first: Placed[Rectangle], second: Placed[Rectangle]): Option[Collision] =
      val centerDistance = second.center - first.center
      val overlaps = (axes(first) ++ axes(second)).map: axis =>
        val normalizedAxis = axis.normalized
        val overlap = projectionRadius(first, normalizedAxis) + projectionRadius(second, normalizedAxis) -
          math.abs(centerDistance dot normalizedAxis)
        (normalizedAxis, overlap)

      Option.when(overlaps.forall(_._2 >= 0)):
        val (axis, penetrationDepth) = overlaps.minBy(_._2)
        val normal = if centerDistance.dot(axis) >= 0 then axis else axis.flip
        val collisionPoint = (supportPoint(first, normal) + supportPoint(second, normal.flip)) * 0.5
        Collision(normal, penetrationDepth, collisionPoint)

  given circleCollidesWithRectangle: Collides[Circle, Rectangle] with

    override def checkCollision(circle: Placed[Circle], rectangle: Placed[Rectangle]): Option[Collision] =
      val localCircle = localPoint(circle.center, rectangle)
      val localClosestPoint = Vector2D(
        clamp(localCircle.x, -rectangle.shape.halfLength, rectangle.shape.halfLength),
        clamp(localCircle.y, -rectangle.shape.halfHeight, rectangle.shape.halfHeight)
      )
      val circleToClosestPoint = localClosestPoint - localCircle
      val distance = circleToClosestPoint.magnitude

      if distance > 0 then
        val penetrationDepth = circle.shape.radius - distance
        Option.when(penetrationDepth >= 0):
          Collision(
            circleToClosestPoint.normalized.rotated(rectangle.rotation),
            penetrationDepth,
            worldPoint(localClosestPoint, rectangle)
          )
      else
        Some(collisionFromCircleInsideRectangle(circle, rectangle, localCircle))

  given rectangleCollidesWithCircle: Collides[Rectangle, Circle] with

    override def checkCollision(rectangle: Placed[Rectangle], circle: Placed[Circle]): Option[Collision] =
      circleCollidesWithRectangle.checkCollision(circle, rectangle)
        .map(collision => collision.copy(normalVector = collision.normalVector.flip))

  given shapeCollidesWithShape: Collides[Shape2D, Shape2D] with

    override def checkCollision(first: Placed[Shape2D], second: Placed[Shape2D]): Option[Collision] =
      (first.shape, second.shape) match
        case (firstCircle: Circle, secondCircle: Circle) =>
          circleCollidesWithCircle.checkCollision(
            Placed(first.center, firstCircle, first.rotation),
            Placed(second.center, secondCircle, second.rotation)
          )
        case (firstRectangle: Rectangle, secondRectangle: Rectangle) =>
          rectangleCollidesWithRectangle.checkCollision(
            Placed(first.center, firstRectangle, first.rotation),
            Placed(second.center, secondRectangle, second.rotation)
          )
        case (circle: Circle, rectangle: Rectangle) =>
          circleCollidesWithRectangle.checkCollision(
            Placed(first.center, circle, first.rotation),
            Placed(second.center, rectangle, second.rotation)
          )
        case (rectangle: Rectangle, circle: Circle) =>
          rectangleCollidesWithCircle.checkCollision(
            Placed(first.center, rectangle, first.rotation),
            Placed(second.center, circle, second.rotation)
          )

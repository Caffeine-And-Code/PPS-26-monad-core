package monad_core.engine.geometry

import monad_core.engine.model.*
import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.physics.pathfinding.RectangleVertexes.vertexes

object ShapeCollision:

  private val Epsilon = 1e-9

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

  private def localPoint(point: Vector2D, reference: Placed[?]): Vector2D =
    (point - reference.center).rotated(-reference.rotation)

  private def worldPoint(point: Vector2D, reference: Placed[?]): Vector2D =
    reference.center + point.rotated(reference.rotation)

  /**
   * Clips a polygon against one boundary represented by a signed-distance function.
   * Each edge crossing is replaced by its linearly interpolated boundary intersection.
   *
   * @param polygon
   *   ordered polygon vertices
   * @param signedDistance
   *   negative-or-zero test for the retained half-plane
   * @return
   *   vertices of the clipped polygon
   */
  private def clipPolygon(
      polygon: List[Vector2D],
      signedDistance: Vector2D => Double
  ): List[Vector2D] =
    polygon.indices.toList.flatMap: index =>
      val current         = polygon(index)
      val next            = polygon((index + 1) % polygon.size)
      val currentDistance = signedDistance(current)
      val nextDistance    = signedDistance(next)
      val currentInside   = currentDistance <= Epsilon
      val nextInside      = nextDistance <= Epsilon

      def intersection: Vector2D =
        val ratio = currentDistance / (currentDistance - nextDistance)
        current + (next - current) * ratio

      if currentInside then
        if nextInside then List(next)
        else List(intersection)
      else if nextInside then List(intersection, next)
      else List.empty

  /**
   * Calculates the center of the intersection polygon between two rectangles.
   * The first rectangle is clipped in the second rectangle's local coordinate system.
   *
   * @param first
   *   first placed rectangle
   * @param second
   *   clipping rectangle and local reference frame
   * @return
   *   intersection centroid in world coordinates
   */
  private[geometry] def intersectionCenter(
      first: Placed[Rectangle],
      second: Placed[Rectangle]
  ): Vector2D =
    val firstInSecondLocal = first.shape
      .vertexes(first.center, first.rotation)
      .map(localPoint(_, second))
    val clipped = Seq[Vector2D => Double](
      _.x - second.shape.halfLength,
      point => -point.x - second.shape.halfLength,
      _.y - second.shape.halfHeight,
      point => -point.y - second.shape.halfHeight
    ).foldLeft(firstInSecondLocal)(clipPolygon)

    if clipped.isEmpty then (first.center + second.center) * 0.5
    else worldPoint(clipped.reduce(_ + _) * (1.0 / clipped.size), second)

  /**
   * Resolves contact data when a circle center lies inside a rectangle.
   * The nearest rectangle edge determines the normal, depth, and contact point.
   *
   * @param circle
   *   placed circle inside the rectangle
   * @param rectangle
   *   containing rectangle
   * @param localCircle
   *   circle center expressed in rectangle-local coordinates
   * @return
   *   collision directed towards the nearest rectangle edge
   */
  private def collisionFromCircleInsideRectangle(
      circle: Placed[Circle],
      rectangle: Placed[Rectangle],
      localCircle: Vector2D
  ): Collision =
    val nearestEdge = Seq(
      (
        rectangle.shape.halfLength - localCircle.x,
        Vector2D(-1, 0),
        Vector2D(rectangle.shape.halfLength, localCircle.y)
      ),
      (
        rectangle.shape.halfLength + localCircle.x,
        Vector2D(1, 0),
        Vector2D(-rectangle.shape.halfLength, localCircle.y)
      ),
      (
        rectangle.shape.halfHeight - localCircle.y,
        Vector2D(0, -1),
        Vector2D(localCircle.x, rectangle.shape.halfHeight)
      ),
      (
        rectangle.shape.halfHeight + localCircle.y,
        Vector2D(0, 1),
        Vector2D(localCircle.x, -rectangle.shape.halfHeight)
      )
    ).minBy(_._1)

    Collision(
      nearestEdge._2.rotated(rectangle.rotation),
      nearestEdge._1 + circle.shape.radius,
      worldPoint(nearestEdge._3, rectangle)
    )

  given circleCollidesWithCircle: Collides[Circle, Circle] with

    override def checkCollision(first: Placed[Circle], second: Placed[Circle]): Option[Collision] =
      val distance         = first.center --> second.center
      val penetrationDepth = first.shape.radius + second.shape.radius - distance

      Option.when(penetrationDepth >= 0):
        val normal =
          if distance <= Epsilon then Vector2D(1, 0)
          else calculateNorm(first.center, second.center)
        val firstContact  = first.center + normal * first.shape.radius
        val secondContact = second.center - normal * second.shape.radius

        Collision(normal, penetrationDepth, (firstContact + secondContact) * 0.5)

  given rectangleCollidesWithRectangle: Collides[Rectangle, Rectangle] with

    override def checkCollision(
        first: Placed[Rectangle],
        second: Placed[Rectangle]
    ): Option[Collision] =
      val centerDistance = second.center - first.center
      val overlaps = (axes(first) ++ axes(second)).map: axis =>
        val normalizedAxis = axis.normalized
        val overlap =
          projectionRadius(first, normalizedAxis) + projectionRadius(second, normalizedAxis) -
            math.abs(centerDistance dot normalizedAxis)
        (normalizedAxis, overlap)

      Option.when(overlaps.forall(_._2 >= 0)):
        val (axis, penetrationDepth) = overlaps.minBy(_._2)
        val normal                   = if centerDistance.dot(axis) >= 0 then axis else axis.flip
        val collisionPoint           = intersectionCenter(first, second)

        Collision(normal, penetrationDepth, collisionPoint)

  given circleCollidesWithRectangle: Collides[Circle, Rectangle] with

    override def checkCollision(
        circle: Placed[Circle],
        rectangle: Placed[Rectangle]
    ): Option[Collision] =
      val localCircle = localPoint(circle.center, rectangle)
      val localClosestPoint = Vector2D(
        clamp(localCircle.x, -rectangle.shape.halfLength, rectangle.shape.halfLength),
        clamp(localCircle.y, -rectangle.shape.halfHeight, rectangle.shape.halfHeight)
      )
      val circleToClosestPoint = localClosestPoint - localCircle
      val distance             = circleToClosestPoint.magnitude

      if distance > 0 then
        val penetrationDepth = circle.shape.radius - distance

        Option.when(penetrationDepth >= 0):
          Collision(
            circleToClosestPoint.normalized.rotated(rectangle.rotation),
            penetrationDepth,
            worldPoint(localClosestPoint, rectangle)
          )
      else Some(collisionFromCircleInsideRectangle(circle, rectangle, localCircle))

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

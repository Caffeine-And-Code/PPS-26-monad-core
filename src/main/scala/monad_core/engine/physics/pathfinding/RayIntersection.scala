package monad_core.engine.physics.pathfinding

import monad_core.engine.model.{-, LocatableId, Vector2D, dot, magnitude}

private[pathfinding] object RayIntersection:

  private[pathfinding] val Epsilon: Double = 1e-8

  def apply(
      rayStart: Vector2D,
      rayDirection: Vector2D,
      vertexMap: Map[LocatableId, List[Vector2D]]
  ): Option[LocatableId] =
    withDistance(rayStart, rayDirection, vertexMap).map(_._1)

  def withDistance(
      rayStart: Vector2D,
      rayDirection: Vector2D,
      vertexMap: Map[LocatableId, List[Vector2D]]
  ): Option[(LocatableId, Double)] =
    vertexMap
      .flatMap { case (id, vertices) =>
        val edges = vertices match
          case first :: remaining => vertices.zip(remaining :+ first)
          case Nil                => List.empty

        val distances =
          edges.flatMap { case (a, b) =>
            raySegmentIntersection(rayStart, rayDirection, a, b)
          } ++ vertices.flatMap(vertex =>
            rayVertexIntersection(
              rayStart,
              rayDirection,
              vertex
            )
          )

        distances
          .filter(_ > Epsilon)
          .minOption
          .map(distance => id -> distance)
      }
      .minByOption(_._2)

  private[pathfinding] def raySegmentIntersection(
      rayStart: Vector2D,
      rayDirection: Vector2D,
      vertex1: Vector2D,
      vertex2: Vector2D
  ): Option[Double] = {

    val segment = vertex2 - vertex1

    val cross = rayDirection.x * segment.y -
      rayDirection.y * segment.x

    if math.abs(cross) < Epsilon then None
    else
      val diff = vertex1 - rayStart

      val distanceOnRay =
        (diff.x * segment.y - diff.y * segment.x) / cross

      val positionOnSegment =
        (diff.x * rayDirection.y - diff.y * rayDirection.x) / cross

      if (distanceOnRay >= 0 && positionOnSegment >= 0 && positionOnSegment <= 1)
        Some(distanceOnRay)
      else
        None
  }

  private[pathfinding] def rayVertexIntersection(
      rayStart: Vector2D,
      rayDirection: Vector2D,
      vertex: Vector2D
  ): Option[Double] =
    val directionMagnitude = rayDirection.magnitude
    val difference         = vertex - rayStart
    val distanceOnRay      = (difference dot rayDirection) / directionMagnitude
    val distanceFromRay = math.abs(
      difference.x * rayDirection.y - difference.y * rayDirection.x
    ) / directionMagnitude

    if distanceOnRay > Epsilon && distanceFromRay <= Epsilon then Some(distanceOnRay)
    else None

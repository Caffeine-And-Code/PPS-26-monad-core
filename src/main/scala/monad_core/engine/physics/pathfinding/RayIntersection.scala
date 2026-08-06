package monad_core.engine.physics.pathfinding

import monad_core.engine.model.{-, LocatableId, Vector2D}

object RayIntersection:

  private val Epsilon: Double = 1e-10
  
  def apply(
                       rayStart: Vector2D,
                       rayDirection: Vector2D,
                       vertexMap: Map[LocatableId, List[Vector2D]]
                     ): Option[LocatableId] = {

    vertexMap
      .flatMap { case (id, vertices) =>

        val edges =
          vertices.zip(vertices.tail :+ vertices.head)

        val distances =
          edges.flatMap { case (a, b) =>
            raySegmentIntersection(
              rayStart,
              rayDirection,
              a,
              b
            )
          }

        distances.minOption.map(distance => id -> distance)
      }
      .minByOption(_._2)
      .map(_._1)
  }

  private def raySegmentIntersection(
                                      rayStart: Vector2D,
                                      rayDirection: Vector2D,
                                      vertex1: Vector2D,
                                      vertex2: Vector2D
                                    ): Option[Double] = {

    val segment = vertex2 - vertex1

    val cross = rayDirection.x * segment.y -
      rayDirection.y * segment.x

    if (math.abs(cross) < Epsilon) then
      None
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

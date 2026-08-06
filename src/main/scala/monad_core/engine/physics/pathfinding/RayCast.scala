package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.physics.core.{PhysicsError, RayIntersectedAMissingEntity, RayIntersectedNothing}

object RayCast:

  private def WayPointDisplacement = 0.1

  def apply(
             to: Entity,
             from: Entity,
             entities: List[Entity],
             entitiesVertexes: Map[LocatableId, List[Vector2D]],
             upperLeftSceneCorner: Vector2D,
             lowerRightSceneCorner: Vector2D
           ): Either[PhysicsError, Option[Vector2D]] =
    val entitiesVertexesWithoutFrom = entitiesVertexes.filterNot(_._1 == from.id)

    val rayDirection = (to.position - from.position).normalized

    val firstObject = RayIntersection(
      from.position,
      rayDirection,
      entitiesVertexesWithoutFrom
    )

    firstObject match
      case Some(first) =>
        if first == to.id then
          Right(Some(to.position))
        else
          val targetEntity = entities.find(_.id == first)

          targetEntity match
            case Some(target) =>
              val bestWaypoint = findBestWaypoint(
                to,
                from,
                entitiesVertexesWithoutFrom(target.id),
                upperLeftSceneCorner,
                lowerRightSceneCorner
              )

              bestWaypoint match
                case Some(waypoint) =>
                  Right(Some(actualWaypoint(to, from, waypoint)))
                case None =>
                  Right(None)
            case None => Left(RayIntersectedAMissingEntity(first.value))
      case None => Left(RayIntersectedNothing(from.id.value, to.id.value))

  private def actualWaypoint(
                              to: Entity,
                              from: Entity,
                              waypoint: Vector2D
                            ): Vector2D = {

    val direction = (waypoint - to.position).normalized

    val horizontal = VertexFinder.horizontalShapeSize(from) / 2
    val vertical = VertexFinder.verticalShapeSize(from) / 2

    val entityDisplacement =
      math.abs(direction.x) * horizontal +
        math.abs(direction.y) * vertical

    waypoint + direction * (WayPointDisplacement + entityDisplacement)
  }

  private def isValidWayPoint(
                       to: Entity,
                       from: Entity,
                       waypoint: Vector2D,
                       upperLeftSceneCorner: Vector2D,
                       lowerRightSceneCorner: Vector2D
                     ): Boolean = {

    val actWaypoint = actualWaypoint(to, from, waypoint)

    val horizontal = VertexFinder.horizontalShapeSize(from) / 2
    val vertical = VertexFinder.verticalShapeSize(from) / 2

    actWaypoint.x - horizontal >= upperLeftSceneCorner.x &&
      actWaypoint.y - vertical >= upperLeftSceneCorner.y &&
      actWaypoint.x + horizontal <= lowerRightSceneCorner.x &&
      actWaypoint.y + vertical <= lowerRightSceneCorner.y
  }

  private def findBestWaypoint(
    to: Entity,
    from: Entity,
    waypoints: List[Vector2D],
    upperLeftSceneCorner: Vector2D,
    lowerRightSceneCorner: Vector2D
  ): Option[Vector2D] = {
    val validWaypoints = waypoints.filter(w => isValidWayPoint(to, from, w, upperLeftSceneCorner, lowerRightSceneCorner))

    if validWaypoints.isEmpty then None
    else Some(validWaypoints.minBy(_.euclideanDistance(to.position)))
  }
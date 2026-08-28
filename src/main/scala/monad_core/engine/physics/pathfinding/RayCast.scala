package monad_core.engine.physics.pathfinding

import monad_core.engine.model.*
import monad_core.engine.physics.core.{
  PhysicsError,
  RayIntersectedAMissingEntity,
  RayIntersectedNothing
}

/** Resolves direct visibility and detour waypoints between two entities. */
private[physics] object RayCast:

  /** Extra clearance applied after accounting for the moving entity radius. */
  private val WayPointDisplacement = 5.0

  /** Safety margin used while inflating obstacles for the ray cast. */
  private val HunterMargin         = 1.0

  /**
   * Casts a ray from one entity towards another.
   *
   * @param to
   *  the entity towards which the ray is cast
   * @param from
   *  the entity from which the ray is cast
   * @param entities
   *  the list of all entities in the world
   * @param entitiesVertexes
   *  the precomputed world-space vertices of all entities in the world
   * @param upperLeftSceneCorner
   *  the upper-left corner of the world bounds
   * @param lowerRightSceneCorner
   *  the lower-right corner of the world bounds
   * @return
   *   the target position when visible, a valid detour waypoint when obstructed,
   *   or a [[PhysicsError]] when the ray cannot be resolved
   */
  def apply(
      to: Entity,
      from: Entity,
      entities: List[Entity],
      entitiesVertexes: Map[LocatableId, List[Vector2D]],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Either[PhysicsError, Option[Vector2D]] =
    val entitiesVertexesWithoutFrom = entitiesVertexes.filterNot(_._1 == from.id)
    val inflatedVertexes = inflateAllVertexes(
      entitiesVertexesWithoutFrom,
      entities,
      hunterRadius(from) + HunterMargin
    )

    val rayDirection = (to.position - from.position).normalized
    val firstObject = RayIntersection.withDistance(
      from.position,
      rayDirection,
      inflatedVertexes
    )

    firstObject match
      case Some((first, _)) =>
        if first == to.id then Right(Some(to.position))
        else
          findEncounteredEntityWayPoint(
            first,
            to,
            from,
            entities,
            upperLeftSceneCorner,
            lowerRightSceneCorner
          )
      case None => Left(RayIntersectedNothing(from.id.value, to.id.value))

  /**
   * Selects the best valid waypoint around the first entity hit by the ray.
   *
   * @param firstEncounteredEntity
   *  the entity that was first hit by the ray
   * @param originalTarget
   *  the original target entity
   * @param from
   *  the entity from which the ray is cast
   * @param entities
   *  the list of all entities in the world
   * @param upperLeftSceneCorner
   *  the upper-left corner of the world bounds
   * @param lowerRightSceneCorner
   *  the lower-right corner of the world bounds
   * @return
   *  the selected waypoint, no waypoint when none is valid, or a [[RayIntersectedAMissingEntity]] error
   */
  private def findEncounteredEntityWayPoint(
      firstEncounteredEntity: LocatableId,
      originalTarget: Entity,
      from: Entity,
      entities: List[Entity],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Either[PhysicsError, Option[Vector2D]] =

    val targetEntity = entities.find(_.id == firstEncounteredEntity)

    targetEntity match
      case Some(target) =>
        val waypoints = WaypointFinder(from, target)

        val bestWaypoint = findBestWaypoint(
          target,
          originalTarget,
          from,
          waypoints,
          upperLeftSceneCorner,
          lowerRightSceneCorner
        )

        Right(bestWaypoint)
      case None => Left(RayIntersectedAMissingEntity(firstEncounteredEntity.value))

  /**
   * Returns the bounding radius required to clear the moving entity shape.
   *
   * @param hunter
   *  the entity whose bounding radius is to be computed
   * @return
   *  the bounding radius required to clear the moving entity shape
   * */
  private[pathfinding] def hunterRadius(hunter: Entity): Double =
    hunter.shape match
      case circle: Shape2D.Circle => circle.radius
      case rectangle: Shape2D.Rectangle =>
        Vector2D(rectangle.halfLength, rectangle.halfHeight).magnitude

  /**
   * Inflates every known obstacle by the supplied clearance.
   *
   * @param vertexes
   *  the map of vertexes to inflate
   * @param entities
   *  the list of all entities in the world
   * @param inflation
   *  the amount by which to inflate each vertex
   * @return
   *  the map of inflated vertexes
   * */
  private[pathfinding] def inflateAllVertexes(
      vertexes: Map[LocatableId, List[Vector2D]],
      entities: List[Entity],
      inflation: Double
  ): Map[LocatableId, List[Vector2D]] =
    val entitiesById = entities.map(entity => entity.id -> entity).toMap

    vertexes.map { case (id, originalVertexes) =>
      id -> entitiesById.get(id).fold(originalVertexes) { entity =>
        inflateVertexes(originalVertexes, entity, inflation)
      }
    }

  /**
   * Inflates all vertices belonging to one entity.
   *
   * @param originalVertexes
   *   original world-space vertices
   * @param entity
   *   entity defining the obstacle shape and placement
   * @param inflation
   *   outward displacement
   * @return
   *   inflated world-space vertices
   */
  private def inflateVertexes(
      originalVertexes: List[Vector2D],
      entity: Entity,
      inflation: Double
  ): List[Vector2D] =
    originalVertexes.map { vertex =>
      inflateSingleVertex(
        originalVertex = vertex,
        entity = entity,
        inflation = inflation
      )
    }

  /**
   * Moves one obstacle vertex outward according to its shape.
   * Circles use the radial direction, rectangles inflate along their rotated local axes.
   *
   * @param originalVertex
   *   vertex to move
   * @param entity
   *   entity defining the obstacle shape and placement
   * @param inflation
   *   outward displacement
   * @return
   *   inflated world-space vertex
   */
  private def inflateSingleVertex(
      originalVertex: Vector2D,
      entity: Entity,
      inflation: Double
  ): Vector2D = {
    val direction = originalVertex - entity.position

    entity.shape match
      case _: Shape2D.Circle =>
        originalVertex + direction.normalized * inflation
      case _: Shape2D.Rectangle =>
        val localDirection = direction.rotated(-entity.rotation)
        val localInflation = Vector2D(
          math.signum(localDirection.x),
          math.signum(localDirection.y)
        ) * inflation
        originalVertex + localInflation.rotated(entity.rotation)
  }

  /**
   * Applies the moving entity clearance to a candidate obstacle waypoint.
   *
   * @param to
   *  target entity associated with the waypoint calculation
   * @param from
   *  the entity from which the waypoint is to be computed
   * @param waypoint
   *  the candidate waypoint around the target entity
   * @return
   *  the waypoint adjusted for the moving entity clearance
   * */
  private[pathfinding] def actualWaypoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D
  ): Vector2D =

    val clearance = hunterRadius(from) + WayPointDisplacement

    inflateSingleVertex(
      originalVertex = waypoint,
      entity = to,
      inflation = clearance
    )

  /**
   * Checks whether the moving entity fits inside the world when centred on a waypoint.
   *
   * @param to
   *  the entity around which the waypoint is to be computed
   * @param from
   *  the entity from which the waypoint is to be computed
   * @param waypoint
   *  the candidate waypoint around the target entity
   * @param upperLeftSceneCorner
   *  the upper-left corner of the world bounds
   * @param lowerRightSceneCorner
   *  the lower-right corner of the world bounds
   * @return
   *  true if the moving entity fits inside the world when centred on the waypoint, false otherwise
   * */
  private[pathfinding] def isValidWayPoint(
      to: Entity,
      from: Entity,
      waypoint: Vector2D,
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Boolean =

    val horizontal = SizeHelper.horizontalShapeSize(from) / 2
    val vertical   = SizeHelper.verticalShapeSize(from) / 2

    waypoint.x - horizontal >= upperLeftSceneCorner.x &&
    waypoint.y - vertical >= upperLeftSceneCorner.y &&
    waypoint.x + horizontal <= lowerRightSceneCorner.x &&
    waypoint.y + vertical <= lowerRightSceneCorner.y

  /**
   * Selects the valid waypoint nearest to the original target.
   *
   * @param to
   *   obstacle around which candidate waypoints are inflated
   * @param originalTarget
   *   destination used to rank valid waypoints
   * @param from
   *   moving entity whose clearance must be respected
   * @param waypoints
   *   candidate obstacle-edge points
   * @param upperLeftSceneCorner
   *   upper-left world boundary
   * @param lowerRightSceneCorner
   *   lower-right world boundary
   * @return
   *   nearest valid waypoint, or `None` when every candidate is outside the world
   */
  private def findBestWaypoint(
      to: Entity,
      originalTarget: Entity,
      from: Entity,
      waypoints: List[Vector2D],
      upperLeftSceneCorner: Vector2D,
      lowerRightSceneCorner: Vector2D
  ): Option[Vector2D] =

    val validWaypoints = waypoints
      .map(w => actualWaypoint(to, from, w))
      .filter(w =>
        isValidWayPoint(
          to,
          from,
          w,
          upperLeftSceneCorner,
          lowerRightSceneCorner
        )
      )

    if validWaypoints.isEmpty then None
    else Some(validWaypoints.minBy(_.euclideanDistance(originalTarget.position)))

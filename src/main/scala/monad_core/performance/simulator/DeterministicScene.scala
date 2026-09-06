package monad_core.performance.simulator

import monad_core.engine.model.*
import monad_core.performance.model.{EnginePerformanceError, EntityCount, PerformanceError}

/**
 * Builds a reproducible full-physics scene for engine performance measurements.
 *
 * Entities alternate shape, team, movement direction and angular direction so the enabled
 * physics rules receive all required properties without introducing randomness.
 *
 * @see
 *   [[monad_core.engine.model.Scene Scene]]
 */
object DeterministicScene:
  private val EntityRadius               = 1.0
  private val EntitySide                 = EntityRadius * 2.0
  private val EntitySpacing              = EntitySide * 0.75
  private val BorderOffset               = EntityRadius / 2.0
  private val EntityWeight               = 2
  private val EntityHealth               = 100
  private val EntityDamage               = 1
  private val LinearSpeed                = 1.0
  private val VerticalDirectionBlockSize = 2
  private val AngularSpeed               = 30.0
  private val RotationStep               = 15.0
  private val RotationStepsPerTurn       = 24
  private val EntityIdPrefix             = "performance-entity"
  private val FirstTeamId                = "performance-team-a"
  private val SecondTeamId               = "performance-team-b"
  private val SurfaceId                  = "performance-surface"
  private val SurfaceFriction            = 0.1
  private val SurfaceForce               = Vector2D(0.25, 0.5)
  private val SurfaceDamage              = 1

  /**
   * Creates a deterministic scene containing the requested number of entities.
   *
   * @param entityCount
   *   number of entities to place in the scene
   * @return
   *   the populated scene, or the first engine error adapted to the performance domain
   * @see
   *   [[monad_core.engine.model.Scene Scene]] and
   *   [[monad_core.performance.model.EnginePerformanceError EnginePerformanceError]]
   */
  def apply(entityCount: EntityCount): Either[PerformanceError, Scene] =
    for
      bounds           <- boundsFor(entityCount)
      sceneWithTeams   <- addTeams(Scene(bounds = bounds))
      surface          <- surfaceFor(bounds)
      sceneWithSurface <- fromEngine(sceneWithTeams.addSurface(surface))
      populatedScene   <- addEntities(sceneWithSurface, entityCount)
    yield populatedScene

  /**
   * Adapts an engine result to the performance error type.
   *
   * @tparam A
   *   successful result type
   * @param result
   *   engine result to adapt
   * @return
   *   the original value or a wrapped engine error
   * @see
   *   [[monad_core.performance.model.EnginePerformanceError EnginePerformanceError]]
   */
  private def fromEngine[A](result: Either[EngineError, A]): Either[PerformanceError, A] =
    result.left.map(EnginePerformanceError.apply)

  /**
   * Calculates bounds large enough for the square-like entity grid.
   *
   * @param entityCount
   *   number of entities that must fit in the world
   * @return
   *   validated world bounds, or an adapted engine error
   * @see
   *   [[monad_core.engine.model.WorldBounds WorldBounds]]
   */
  private def boundsFor(entityCount: EntityCount): Either[PerformanceError, WorldBounds] =
    val columns = columnsFor(entityCount)
    val rows    = math.ceil(entityCount.value.toDouble / columns).toInt
    fromEngine(WorldBounds(dimensionFor(columns), dimensionFor(rows)))

  /**
   * Adds two mutually hostile teams to a scene.
   *
   * @param initialScene
   *   scene without performance teams
   * @return
   *   the scene containing both teams, or the first adapted engine error
   * @see
   *   [[monad_core.engine.model.Team Team]]
   */
  private def addTeams(initialScene: Scene): Either[PerformanceError, Scene] =
    for
      firstTeam  <- fromEngine(Team.create(FirstTeamId, Set(SecondTeamId)))
      secondTeam <- fromEngine(Team.create(SecondTeamId, Set(FirstTeamId)))
      withFirst  <- fromEngine(initialScene.addTeam(firstTeam))
      withBoth   <- fromEngine(withFirst.addTeam(secondTeam))
    yield withBoth

  /**
   * Creates a surface covering the world and carrying all supported surface properties.
   *
   * @param bounds
   *   bounds used to derive surface size and center
   * @return
   *   the configured surface, or the first adapted engine error
   * @see
   *   [[monad_core.engine.model.Surface Surface]]
   */
  private def surfaceFor(bounds: WorldBounds): Either[PerformanceError, Surface] =
    val width  = bounds.lowerRight.x - bounds.upperLeft.x
    val height = bounds.lowerRight.y - bounds.upperLeft.y
    val center = Vector2D(
      bounds.upperLeft.x + width / 2.0,
      bounds.upperLeft.y + height / 2.0
    )

    fromEngine(
      for
        surface      <- Surface.rectangle(SurfaceId, center, height, width)
        withFriction <- surface.withFrictionIndex(Some(SurfaceFriction))
        withForce    <- withFriction.withAppliedForce(Some(SurfaceForce))
        withDamage   <- withForce.withDamageOverTime(Some(SurfaceDamage))
      yield withDamage
    )

  /**
   * Adds every deterministic entity to a scene in index order.
   *
   * @param initialScene
   *   scene to populate
   * @param entityCount
   *   number of entities to add
   * @return
   *   the populated scene, or the first adapted engine error
   */
  private def addEntities(
      initialScene: Scene,
      entityCount: EntityCount
  ): Either[PerformanceError, Scene] =
    val columns = columnsFor(entityCount)

    (0 until entityCount.value).foldLeft(
      Right(initialScene): Either[PerformanceError, Scene]
    ) { (sceneResult, index) =>
      for
        scene        <- sceneResult
        entity       <- entityAt(index, columns)
        updatedScene <- fromEngine(scene.addEntity(entity))
      yield updatedScene
    }

  /**
   * Creates one fully configured entity at its deterministic grid position.
   *
   * @param index
   *   zero-based entity index
   * @param columns
   *   number of columns in the entity grid
   * @return
   *   the configured entity, or the first adapted engine error
   * @see
   *   [[monad_core.engine.model.Entity Entity]]
   */
  private def entityAt(index: Int, columns: Int): Either[PerformanceError, Entity] =
    val position = Vector2D(
      coordinateFor(index % columns),
      coordinateFor(index / columns)
    )
    val rotation = (index % RotationStepsPerTurn) * RotationStep

    for
      entity       <- fromEngine(entityShapeAt(index, position, rotation))
      weighted     <- fromEngine(entity.withWeight(Some(EntityWeight)))
      healthy      <- fromEngine(weighted.withHealth(Some(EntityHealth)))
      damaging     <- fromEngine(healthy.withDamage(Some(EntityDamage)))
      assignedTeam <- fromEngine(damaging.withTeamId(Some(teamIdFor(index))))
    yield assignedTeam
      .withSpeed(Some(speedFor(index)))
      .withAngularSpeed(Some(angularSpeedFor(index)))

  /**
   * Alternates circular and rectangular entity shapes.
   *
   * @param index
   *   zero-based entity index
   * @param position
   *   entity centre
   * @param rotation
   *   initial rotation in degrees
   * @return
   *   the created entity, or an engine validation error
   * @see
   *   [[monad_core.engine.model.Entity Entity]]
   */
  private def entityShapeAt(
      index: Int,
      position: Vector2D,
      rotation: Double
  ): Either[EngineError, Entity] =
    if isEven(index) then Entity.circle(entityIdFor(index), position, EntityRadius, rotation)
    else Entity.rectangle(entityIdFor(index), position, EntitySide, EntitySide, rotation)

  /**
   * Calculates alternating horizontal and vertical velocity components.
   *
   * @param index
   *   zero-based entity index
   * @return
   *   deterministic linear velocity
   */
  private def speedFor(index: Int): Vector2D =
    Vector2D(
      directionFor(index) * LinearSpeed,
      directionFor(index / VerticalDirectionBlockSize) * LinearSpeed
    )

  /**
   * Calculates an alternating angular velocity.
   *
   * @param index
   *   zero-based entity index
   * @return
   *   angular velocity in degrees per time unit
   */
  private def angularSpeedFor(index: Int): Double =
    directionFor(index) * AngularSpeed

  /**
   * Selects one of the two teams according to entity-index parity.
   *
   * @param index
   *   zero-based entity index
   * @return
   *   deterministic team identifier
   */
  private def teamIdFor(index: Int): String =
    if isEven(index) then FirstTeamId else SecondTeamId

  /**
   * Creates a stable identifier from an entity index.
   *
   * @param index
   *   zero-based entity index
   * @return
   *   unique performance entity identifier
   */
  private def entityIdFor(index: Int): String =
    s"$EntityIdPrefix-$index"

  /**
   * Converts block parity into a positive or negative direction.
   *
   * @param block
   *   block whose parity determines the direction
   * @return
   *   `1.0` for even blocks or `-1.0` for odd blocks
   */
  private def directionFor(block: Int): Double =
    if isEven(block) then 1.0 else -1.0

  /**
   * Checks integer parity.
   *
   * @param value
   *   integer to inspect
   * @return
   *   whether `value` is even
   */
  private def isEven(value: Int): Boolean =
    value % 2 == 0

  /**
   * Calculates the grid width as the ceiling of the square root of the entity count.
   *
   * @param entityCount
   *   number of entities in the grid
   * @return
   *   required number of columns
   */
  private def columnsFor(entityCount: EntityCount): Int =
    math.ceil(math.sqrt(entityCount.value.toDouble)).toInt

  /**
   * Calculates one grid coordinate from its zero-based index.
   *
   * @param index
   *   row or column index
   * @return
   *   coordinate including border offset and entity spacing
   */
  private def coordinateFor(index: Int): Double =
    BorderOffset + index * EntitySpacing

  /**
   * Calculates a world dimension for a number of grid cells.
   *
   * @param cellCount
   *   number of cells along one axis
   * @return
   *   dimension including spacing and offsets from both borders
   */
  private def dimensionFor(cellCount: Int): Double =
    BorderOffset * 2.0 + (cellCount - 1) * EntitySpacing

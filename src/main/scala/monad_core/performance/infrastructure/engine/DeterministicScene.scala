package monad_core.performance.infrastructure.engine

import monad_core.engine.model.*
import monad_core.performance.domain.{EnginePerformanceError, EntityCount, PerformanceError}

/**
 * Builds reproducible, full-physics engine scenes for performance measurements.
 *
 * Every entity has complete linear, angular, health, and damage properties. Alternating shapes,
 * teams, and movement directions exercise collision resolution, enemy attraction, damage
 * application, and kinematics. The overlapping grid crosses every world border and is covered by
 * one damaging surface, so the default physics pipeline can be measured as one integrated workload.
 */
object DeterministicScene:

  /** Radius assigned to generated circles. */
  private val EntityRadius = 1.0

  /** Side assigned to generated squares. */
  private val EntitySide = EntityRadius * 2.0

  /** Fraction of one entity side separating adjacent centers. */
  private val EntitySpacingRatio = 0.75

  /** Distance between adjacent grid centers, intentionally smaller than one entity side. */
  private val EntitySpacing = EntitySide * EntitySpacingRatio

  /** Offset that makes edge entities cross the corresponding world border. */
  private val BorderOffset = EntityRadius / 2.0

  /** Positive mass assigned to every entity. */
  private val EntityWeight = 2

  /** Health high enough to survive one complete integrated tick. */
  private val EntityHealth = 100

  /** Contact damage inflicted by every entity. */
  private val EntityDamage = 1

  /** Magnitude used by both components of linear velocity. */
  private val LinearSpeed = 1.0

  /** Number of consecutive entities sharing one vertical direction. */
  private val VerticalDirectionBlockSize = 2

  /** Magnitude of angular velocity in degrees per second. */
  private val AngularSpeed = 30.0

  /** Difference in degrees between consecutive initial rotations. */
  private val RotationStep = 15.0

  /** Number of distinct rotation steps in one full turn. */
  private val RotationStepsPerTurn = 24

  /** Stable prefix used to derive unique entity identifiers. */
  private val EntityIdPrefix = "performance-entity"

  /** Stable identifier of the first opposing team. */
  private val FirstTeamId = "performance-team-a"

  /** Stable identifier of the second opposing team. */
  private val SecondTeamId = "performance-team-b"

  /** Stable identifier of the full-scene surface. */
  private val SurfaceId = "performance-surface"

  /** Friction applied to entities by the full-scene surface. */
  private val SurfaceFriction = 0.1

  /** Force applied to entities by the full-scene surface. */
  private val SurfaceForce = Vector2D(0.25, 0.5)

  /** Contact damage inflicted by the full-scene surface. */
  private val SurfaceDamage = 1

  /**
   * Creates a deterministic scene containing exactly the requested number of entities.
   *
   * @param entityCount
   *   validated number of entities to create
   * @return
   *   the complete scene, or the first translated engine error
   */
  def apply(entityCount: EntityCount): Either[PerformanceError, Scene] =
    for
      bounds           <- boundsFor(entityCount)
      sceneWithTeams   <- addTeams(Scene(bounds = bounds))
      surface          <- surfaceFor(bounds)
      sceneWithSurface <- fromEngine(sceneWithTeams.addSurface(surface))
      populatedScene   <- addEntities(sceneWithSurface, entityCount)
    yield populatedScene

  /** Translates one engine result into the performance error domain. */
  private def fromEngine[A](result: Either[EngineError, A]): Either[PerformanceError, A] =
    result.left.map(EnginePerformanceError.apply)

  /** Calculates tight world bounds for the overlapping entity grid. */
  private def boundsFor(entityCount: EntityCount): Either[PerformanceError, WorldBounds] =
    val columns = columnsFor(entityCount)
    val rows    = math.ceil(entityCount.value.toDouble / columns).toInt
    fromEngine(WorldBounds(dimensionFor(columns), dimensionFor(rows)))

  /** Creates and inserts both mutually opposing teams. */
  private def addTeams(initialScene: Scene): Either[PerformanceError, Scene] =
    for
      firstTeam  <- fromEngine(Team.create(FirstTeamId, Set(SecondTeamId)))
      secondTeam <- fromEngine(Team.create(SecondTeamId, Set(FirstTeamId)))
      withFirst  <- fromEngine(initialScene.addTeam(firstTeam))
      withBoth   <- fromEngine(withFirst.addTeam(secondTeam))
    yield withBoth

  /** Creates the rectangle that applies force, friction, and damage across the whole world. */
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
        withFriction <- surface.withFrictionIndex(SurfaceFriction)
        withForce    <- withFriction.withAppliedForce(SurfaceForce)
        withDamage   <- withForce.withDamageOverTime(SurfaceDamage)
      yield withDamage
    )

  /** Adds every deterministic entity in index order, stopping at the first engine error. */
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

  /** Creates one fully configured entity at its deterministic grid index. */
  private def entityAt(index: Int, columns: Int): Either[PerformanceError, Entity] =
    val position = Vector2D(
      coordinateFor(index % columns),
      coordinateFor(index / columns)
    )
    val rotation = (index % RotationStepsPerTurn) * RotationStep

    for
      entity       <- fromEngine(entityShapeAt(index, position, rotation))
      weighted     <- fromEngine(entity.withWeight(EntityWeight))
      healthy      <- fromEngine(weighted.withHealth(EntityHealth))
      damaging     <- fromEngine(healthy.withDamage(EntityDamage))
      assignedTeam <- fromEngine(damaging.withTeamId(teamIdFor(index)))
    yield assignedTeam
      .withSpeed(speedFor(index))
      .withAngularSpeed(angularSpeedFor(index))

  /** Alternates circles and squares so both engine shape families are represented. */
  private def entityShapeAt(
      index: Int,
      position: Vector2D,
      rotation: Double
  ): Either[EngineError, Entity] =
    if isEven(index) then
      Entity.circle(entityIdFor(index), position, EntityRadius, rotation)
    else
      Entity.rectangle(entityIdFor(index), position, EntitySide, EntitySide, rotation)

  /** Produces a non-zero two-dimensional velocity with deterministic alternating directions. */
  private def speedFor(index: Int): Vector2D =
    Vector2D(
      directionFor(index) * LinearSpeed,
      directionFor(index / VerticalDirectionBlockSize) * LinearSpeed
    )

  /** Alternates clockwise and counter-clockwise angular velocity. */
  private def angularSpeedFor(index: Int): Double =
    directionFor(index) * AngularSpeed

  /** Alternates entity membership between the two opposing teams. */
  private def teamIdFor(index: Int): String =
    if isEven(index) then FirstTeamId else SecondTeamId

  /** Derives a stable entity identifier from its zero-based index. */
  private def entityIdFor(index: Int): String =
    s"$EntityIdPrefix-$index"

  /** Returns a positive sign for even blocks and a negative sign for odd blocks. */
  private def directionFor(block: Int): Double =
    if isEven(block) then 1.0 else -1.0

  /** Tests index parity without duplicating modulo expressions. */
  private def isEven(value: Int): Boolean =
    value % 2 == 0

  /** Selects a near-square positive column count. */
  private def columnsFor(entityCount: EntityCount): Int =
    math.ceil(math.sqrt(entityCount.value.toDouble)).toInt

  /** Converts one zero-based grid index into a world coordinate. */
  private def coordinateFor(index: Int): Double =
    BorderOffset + index * EntitySpacing

  /** Fits the outer centers while leaving every edge entity across its border. */
  private def dimensionFor(cellCount: Int): Double =
    BorderOffset * 2.0 + (cellCount - 1) * EntitySpacing

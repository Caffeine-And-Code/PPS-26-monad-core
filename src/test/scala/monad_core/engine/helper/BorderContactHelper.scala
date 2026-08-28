package monad_core.engine.helper

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{BorderSide, Entity, Vector2D}
import PhysicsConstantHelper.DefaultRadius
import helpers.dummies.DummyEntityHelper.makeMovingEntityCircle
import monad_core.engine.physics.utils.BorderWall
import org.scalatest.EitherValues.convertEitherToValuable

/** Fixture containing one entity-to-border collision and its expected response. */
private[engine] case class SingleWallResult(
    entity: Entity,
    wall: Entity,
    collision: Collision,
    expectedPosition: Vector2D,
    expectedSpeed: Vector2D
)

/** Fixture containing two border collisions at one world corner and their expected response. */
private[engine] case class CornerWallResult(
    entity: Entity,
    wall1: Entity,
    collision1: Collision,
    wall2: Entity,
    collision2: Collision,
    expectedPosition: Vector2D,
    expectedSpeed: Vector2D
)

/** Builds deterministic entity and wall fixtures for border-contact tests. */
private[engine] object BorderContactHelper:

  /**
   * Creates a circular entity crossing one selected border.
   *
   * @param borderSide
   *   border crossed by the entity
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @param entityId
   *   fixture entity identifier
   * @return
   *   generated entity, wall contact, and expected response
   */
  def generateSingleWallEntities(
      borderSide: BorderSide,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      entityId: String = "entity"
  ): SingleWallResult =
    val values = borderSide match
      case BorderSide.Left =>
        (
          Vector2D(upperLeft.x - 10, upperLeft.y + DefaultRadius),
          Vector2D(-1, 0),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(1, 0)
        )
      case BorderSide.Right =>
        (
          Vector2D(lowerRight.x + 10, lowerRight.y - DefaultRadius),
          Vector2D(1, 0),
          Vector2D(lowerRight.x - DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(-1, 0)
        )
      case BorderSide.Top =>
        (
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y - 10),
          Vector2D(0, -1),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(0, 1)
        )
      case BorderSide.Bottom =>
        (
          Vector2D(lowerRight.x - DefaultRadius, lowerRight.y + 10),
          Vector2D(0, 1),
          Vector2D(lowerRight.x - DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(0, -1)
        )

    val entity = makeMovingEntityCircle(
      id = entityId,
      position = values._1,
      radius = DefaultRadius
    ).withSpeed(values._2)

    val wallCollision = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      upperLeft,
      lowerRight,
      borderSide
    ).value

    SingleWallResult(entity, wallCollision._1, wallCollision._2, values._3, values._4)

  /**
   * Creates a circular entity crossing one vertical and one horizontal border.
   *
   * @param borderSideV
   *   crossed vertical border
   * @param borderSideH
   *   crossed horizontal border
   * @param upperLeft
   *   upper-left world boundary
   * @param lowerRight
   *   lower-right world boundary
   * @return
   *   generated entity, both wall contacts, and expected response
   */
  def generateCornerEntities(
      borderSideV: BorderSide,
      borderSideH: BorderSide,
      upperLeft: Vector2D,
      lowerRight: Vector2D
  ): CornerWallResult =

    val values = (borderSideV, borderSideH) match
      case (BorderSide.Left, BorderSide.Top) =>
        (
          Vector2D(upperLeft.x - 10, upperLeft.y - 10),
          Vector2D(-1, -1),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(1, 1)
        )
      case (BorderSide.Left, BorderSide.Bottom) =>
        (
          Vector2D(upperLeft.x - 10, lowerRight.y + 10),
          Vector2D(-1, 1),
          Vector2D(upperLeft.x + DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(1, -1)
        )
      case (BorderSide.Right, BorderSide.Top) =>
        (
          Vector2D(lowerRight.x + 10, upperLeft.y - 10),
          Vector2D(1, -1),
          Vector2D(lowerRight.x - DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(-1, 1)
        )
      case (BorderSide.Right, BorderSide.Bottom) =>
        (
          Vector2D(lowerRight.x + 10, lowerRight.y + 10),
          Vector2D(1, 1),
          Vector2D(lowerRight.x - DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(-1, -1)
        )
      case _ =>
        (
          Vector2D(0, 0),
          Vector2D(0, 0),
          Vector2D(0, 0),
          Vector2D(0, 0)
        )

    val entity = makeMovingEntityCircle(
      position = values._1,
      radius = DefaultRadius
    ).withSpeed(values._2)

    val verticalWall = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      upperLeft,
      lowerRight,
      borderSideV
    ).value

    val horizontalWall = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      upperLeft,
      lowerRight,
      borderSideH
    ).value

    CornerWallResult(
      entity,
      verticalWall._1,
      verticalWall._2,
      horizontalWall._1,
      horizontalWall._2,
      values._3,
      values._4
    )

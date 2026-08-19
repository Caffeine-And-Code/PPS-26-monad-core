package monad_core.engine.helper

import monad_core.engine.geometry.Collision
import monad_core.engine.model.{Entity, Vector2D}
import PhysicsConstantHelper.{DefaultRadius, DeltaTimeOneSecond}
import DummyEntityHelper.makeMovingEntityCircle
import monad_core.engine.physics.utils.{BorderWall, BorderWallType}
import org.scalatest.EitherValues.convertEitherToValuable

private[engine] object BorderContactHelper:

  def generateSingleWallEntities(
      borderType: BorderWallType,
      upperLeft: Vector2D,
      lowerRight: Vector2D,
      entityId: String = "entity"
  ): (
      Entity,
      Entity,
      Collision,
      Vector2D,
      Vector2D
  ) =
    val values = borderType match
      case BorderWallType.Left =>
        (
          Vector2D(upperLeft.x - 10, upperLeft.y + DefaultRadius),
          Vector2D(-1, 0),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(1, 0)
        )
      case BorderWallType.Right =>
        (
          Vector2D(lowerRight.x + 10, lowerRight.y - DefaultRadius),
          Vector2D(1, 0),
          Vector2D(lowerRight.x - DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(-1, 0)
        )
      case BorderWallType.Top =>
        (
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y - 10),
          Vector2D(0, -1),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(0, 1)
        )
      case BorderWallType.Bottom =>
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
      borderType
    ).value

    (entity, wallCollision._1, wallCollision._2, values._3, values._4)

  def generateCornerEntities(
      borderTypeV: BorderWallType,
      borderTypeH: BorderWallType,
      upperLeft: Vector2D,
      lowerRight: Vector2D
  ): (
      Entity,
      Entity,
      Collision,
      Entity,
      Collision,
      Vector2D,
      Vector2D
  ) =

    val values = (borderTypeV, borderTypeH) match
      case (BorderWallType.Left, BorderWallType.Top) =>
        (
          Vector2D(upperLeft.x - 10, upperLeft.y - 10),
          Vector2D(-1, -1),
          Vector2D(upperLeft.x + DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(1, 1)
        )
      case (BorderWallType.Left, BorderWallType.Bottom) =>
        (
          Vector2D(upperLeft.x - 10, lowerRight.y + 10),
          Vector2D(-1, 1),
          Vector2D(upperLeft.x + DefaultRadius, lowerRight.y - DefaultRadius),
          Vector2D(1, -1)
        )
      case (BorderWallType.Right, BorderWallType.Top) =>
        (
          Vector2D(lowerRight.x + 10, upperLeft.y - 10),
          Vector2D(1, -1),
          Vector2D(lowerRight.x - DefaultRadius, upperLeft.y + DefaultRadius),
          Vector2D(-1, 1)
        )
      case (BorderWallType.Right, BorderWallType.Bottom) =>
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
      borderTypeV
    ).value

    val horizontalWall = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      upperLeft,
      lowerRight,
      borderTypeH
    ).value

    (
      entity,
      verticalWall._1,
      verticalWall._2,
      horizontalWall._1,
      horizontalWall._2,
      values._3,
      values._4
    )

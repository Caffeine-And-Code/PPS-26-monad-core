package monad_core.engine.physics.utils

import monad_core.engine.helper.DummyEntityHelper.{
  makeMovingEntityCircle,
  makeMovingEntityRectangle
}
import monad_core.engine.model.Vector2D
import monad_core.engine.helper.PhysicsConstantHelper.DefaultRadius
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import monad_core.engine.physics.pathfinding.VertexFinder
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable

class BorderWallTest extends AnyFunSuite with Matchers:

  private val UpperLeftCorner  = Vector2D(0, 0)
  private val LowerRightCorner = Vector2D(100, 100)

  test("this function should create an entity for a margin on the left side of the scene"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(-10, 0),
      radius = DefaultRadius,
      speed = Vector2D(-1, 0)
    )

    val leftWallCollision = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      UpperLeftCorner,
      LowerRightCorner,
      BorderWallType.Left
    ).value

    val wall             = leftWallCollision._1
    val collision        = leftWallCollision._2
    val leftWallVertexes = VertexFinder(List(wall)).get(wall.id).value
    val leftWallLimit    = leftWallVertexes.map(_.x).min

    leftWallVertexes.count(_.x == UpperLeftCorner.x) shouldBe 2
    leftWallVertexes.count(_.x == leftWallLimit) shouldBe 2
    leftWallLimit < (entity.position.x - DefaultRadius) shouldBe true
    collision.penetrationDepth shouldBe math.abs(
      UpperLeftCorner.x - (entity.position.x - DefaultRadius)
    )
    collision.normalVector shouldBe Vector2D(1, 0)
    collision.collisionPoint shouldBe Vector2D(UpperLeftCorner.x, entity.position.y)

  test("this function should create an entity for a margin on the right side of the scene"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(110, 0),
      radius = DefaultRadius,
      speed = Vector2D(1, 0)
    )

    val rightWallCollision = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      UpperLeftCorner,
      LowerRightCorner,
      BorderWallType.Right
    ).value

    val wall              = rightWallCollision._1
    val collision         = rightWallCollision._2
    val rightWallVertexes = VertexFinder(List(wall)).get(wall.id).value
    val rightWallLimit    = rightWallVertexes.map(_.x).max

    rightWallVertexes.count(_.x == LowerRightCorner.x) shouldBe 2
    rightWallVertexes.count(_.x == rightWallLimit) shouldBe 2
    rightWallLimit > (entity.position.x + DefaultRadius) shouldBe true
    collision.penetrationDepth shouldBe math.abs(
      LowerRightCorner.x - (entity.position.x + DefaultRadius)
    )
    collision.normalVector shouldBe Vector2D(-1, 0)
    collision.collisionPoint shouldBe Vector2D(LowerRightCorner.x, entity.position.y)

  test("this function should create an entity for a margin on the top side of the scene"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, -10),
      radius = DefaultRadius,
      speed = Vector2D(0, -1)
    )

    val topWallCollision = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      UpperLeftCorner,
      LowerRightCorner,
      BorderWallType.Top
    ).value

    val wall            = topWallCollision._1
    val collision       = topWallCollision._2
    val topWallVertexes = VertexFinder(List(wall)).get(wall.id).value
    val topWallLimit    = topWallVertexes.map(_.y).min

    topWallVertexes.count(_.y == UpperLeftCorner.y) shouldBe 2
    topWallVertexes.count(_.y == topWallLimit) shouldBe 2
    topWallLimit < (entity.position.y - DefaultRadius) shouldBe true
    collision.penetrationDepth shouldBe math.abs(
      UpperLeftCorner.y - (entity.position.y - DefaultRadius)
    )
    collision.normalVector shouldBe Vector2D(0, 1)
    collision.collisionPoint shouldBe Vector2D(entity.position.x, UpperLeftCorner.y)

  test("this function should create an entity for a margin on the bottom side of the scene"):

    val entity = makeMovingEntityCircle(
      position = Vector2D(0, 110),
      radius = DefaultRadius,
      speed = Vector2D(0, 1)
    )

    val bottomWallCollision = BorderWall(
      entity,
      DefaultRadius,
      DefaultRadius,
      UpperLeftCorner,
      LowerRightCorner,
      BorderWallType.Bottom
    ).value

    val wall               = bottomWallCollision._1
    val collision          = bottomWallCollision._2
    val bottomWallVertexes = VertexFinder(List(wall)).get(wall.id).value
    val bottomWallLimit    = bottomWallVertexes.map(_.y).max

    bottomWallVertexes.count(_.y == LowerRightCorner.y) shouldBe 2
    bottomWallVertexes.count(_.y == bottomWallLimit) shouldBe 2
    bottomWallLimit > (entity.position.y + DefaultRadius) shouldBe true
    collision.penetrationDepth shouldBe math.abs(
      LowerRightCorner.y - (entity.position.y + DefaultRadius)
    )
    collision.normalVector shouldBe Vector2D(0, -1)
    collision.collisionPoint shouldBe Vector2D(entity.position.x, LowerRightCorner.y)

  test("the collision point should be the outermost vertex of a rotated rectangle"):
    val entity = makeMovingEntityRectangle(
      position = Vector2D(10, 50),
      width = 20,
      height = 10,
      speed = Vector2D(-1, 0)
    ).rotateTo(30)

    val horizontalHalfSize = 10 * math.cos(math.toRadians(30)) + 5 * math.sin(math.toRadians(30))
    val verticalHalfSize   = 10 * math.sin(math.toRadians(30)) + 5 * math.cos(math.toRadians(30))

    val collision = BorderWall(
      entity,
      horizontalHalfSize,
      verticalHalfSize,
      UpperLeftCorner,
      LowerRightCorner,
      BorderWallType.Left
    ).value._2

    collision.collisionPoint.x shouldBe UpperLeftCorner.x
    collision.collisionPoint.y shouldBe (50 - 10 * math.sin(math.toRadians(30)) +
      5 * math.cos(math.toRadians(30))) +- 1e-9

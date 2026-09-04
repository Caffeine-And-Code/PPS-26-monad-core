package monad_core.engine.physics.utils

import helpers.dummies.DummyEntityHelper.{makeMovingEntityCircle, makeMovingEntityRectangle}
import monad_core.engine.model.{BorderSide, Vector2D}
import monad_core.engine.helper.PhysicsConstantHelper.DefaultRadius
import monad_core.engine.model.Shape2D.Rectangle
import monad_core.engine.physics.pathfinding.RectangleVertexes.{leftVertex, vertexes}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import monad_core.engine.physics.pathfinding.{SizeHelper, VertexFinder}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable

class BorderWallTest extends AnyFunSuite with Matchers:

  private val UpperLeftCorner  = Vector2D(0, 0)
  private val LowerRightCorner = Vector2D(100, 100)
  private val Epsilon          = 1e-9

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
      BorderSide.Left
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
      BorderSide.Right
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
      BorderSide.Top
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
      BorderSide.Bottom
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
      speed = Vector2D(-1, 0),
      rotation = 30
    )

    val rectangle = entity.shape.asInstanceOf[Rectangle]

    val horizontalHalfSize = SizeHelper.horizontalShapeSize(entity) / 2.0
    val verticalHalfSize   = SizeHelper.verticalShapeSize(entity) / 2.0

    val expectedSupportVertex = rectangle.leftVertex(entity.position, entity.rotation)

    val collision = BorderWall(
      entity,
      horizontalHalfSize,
      verticalHalfSize,
      UpperLeftCorner,
      LowerRightCorner,
      BorderSide.Left
    ).value._2

    collision.collisionPoint.x shouldBe UpperLeftCorner.x
    collision.collisionPoint.y shouldBe expectedSupportVertex.y +- Epsilon

  test("the collision point includes support vertices exactly within epsilon"):

    val entity = makeMovingEntityRectangle(
      position = Vector2D(0.0, 50.0),
      width = Epsilon,
      height = 2.0,
      speed = Vector2D(-1, 0),
      rotation = math.toDegrees(math.asin(Epsilon / 2.0))
    )

    val rectangle = entity.shape.asInstanceOf[Rectangle]
    val vertexes  = rectangle.vertexes(entity.position, entity.rotation)
    val leftmostX = rectangle.leftVertex(entity.position, entity.rotation).x

    val expectedSupportVertices =
      vertexes.filter(vertex => vertex.x - leftmostX <= Epsilon)

    val expectedSupportY =
      expectedSupportVertices.map(_.y).sum / expectedSupportVertices.size

    val collision = BorderWall(
      entity,
      horizontalHalfSize = rectangle.height / 2.0,
      verticalHalfSize = rectangle.height / 2.0,
      upperLeft = UpperLeftCorner,
      lowerRight = LowerRightCorner,
      borderSide = BorderSide.Left
    ).value._2

    expectedSupportVertices should have size 3
    collision.collisionPoint.y shouldBe expectedSupportY +- Epsilon

package monad_core.performance.infrastructure.engine

import monad_core.engine.model.{Entity, Scene, Shape2D, Vector2D}
import monad_core.performance.domain.EntityCount
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DeterministicSceneTest extends AnyFunSuite with Matchers:

  private val EntityNumber          = 5
  private val EntityIdPrefix        = "performance-entity"
  private val EntityRadius          = 1.0
  private val EntityHorizontalSpeed = 1.0
  private val ExpectedUpperLeft     = Vector2D(0.0, 0.0)
  private val ExpectedLowerRight    = Vector2D(20.0, 16.0)

  private val ExpectedPositions = Set(
    Vector2D(4.0, 4.0),
    Vector2D(8.0, 4.0),
    Vector2D(12.0, 4.0),
    Vector2D(4.0, 8.0),
    Vector2D(8.0, 8.0)
  )

  private val EntityCountValue = EntityCount.from(EntityNumber).value

  private def entityIds(scene: Scene): Set[String] =
    scene.allEntities.map(_.id.value).toSet

  private def sceneSnapshot(scene: Scene): (Set[Entity], Vector2D, Vector2D) =
    (scene.allEntities.toSet, scene.bounds.upperLeft, scene.bounds.lowerRight)

  test("a deterministic scene contains the requested number of entities"):

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.size) shouldBe Right(EntityNumber)

  test("a deterministic scene produces the same scene for the same entity count"):

    val first  = DeterministicScene(EntityCountValue)
    val second = DeterministicScene(EntityCountValue)

    first.map(sceneSnapshot) shouldBe second.map(sceneSnapshot)

  test("a deterministic scene assigns a stable identifier to every entity"):
    val expectedIds = (0 until EntityNumber).map(index => s"$EntityIdPrefix-$index").toSet

    val result = DeterministicScene(EntityCountValue)

    result.map(entityIds) shouldBe Right(expectedIds)

  test("a deterministic scene assigns a unique identifier to every entity"):
    val entityCount = EntityCountValue

    val result = DeterministicScene(entityCount)

    result.map(scene => entityIds(scene).size) shouldBe Right(entityCount.value)

  test("a deterministic scene creates equal circular entities"):
    val expectedShape = Shape2D.circle(EntityRadius).value

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.shape).toSet) shouldBe Right(Set(expectedShape))

  test("a deterministic scene gives every entity the same horizontal speed"):
    val expectedSpeed = Some(Vector2D(EntityHorizontalSpeed, 0.0))

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.speed).toSet) shouldBe Right(Set(expectedSpeed))

  test("a deterministic scene arranges entities row by row on a grid"):
    val expectedPositions = ExpectedPositions

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.position).toSet) shouldBe Right(expectedPositions)

  test("a deterministic scene derives bounds from its entity grid"):
    val expectedBounds = (ExpectedUpperLeft, ExpectedLowerRight)

    val result = DeterministicScene(EntityCountValue)

    result.map(scene => (scene.bounds.upperLeft, scene.bounds.lowerRight)) shouldBe Right(
      expectedBounds
    )

  test("a deterministic scene places every entity inside the world bounds"):
    val entityRadius = EntityRadius

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.forall { entity =>
        entity.position.x - entityRadius >= scene.bounds.upperLeft.x &&
        entity.position.y - entityRadius >= scene.bounds.upperLeft.y &&
        entity.position.x + entityRadius <= scene.bounds.lowerRight.x &&
        entity.position.y + entityRadius <= scene.bounds.lowerRight.y
      }
    } shouldBe Right(true)

  test("a deterministic scene does not contain teams"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allTeams) shouldBe Right(List.empty)

  test("a deterministic scene does not contain surfaces"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allSurfaces) shouldBe Right(List.empty)

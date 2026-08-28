package monad_core.performance.infrastructure.engine

import monad_core.engine.model.*
import monad_core.performance.domain.EntityCount
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DeterministicSceneTest extends AnyFunSuite with Matchers:

  private val EntityNumber          = 6
  private val MinimumEntityNumber   = 1
  private val EntityIdPrefix        = "performance-entity"
  private val FirstTeamId           = "performance-team-a"
  private val SecondTeamId          = "performance-team-b"
  private val SurfaceId             = "performance-surface"
  private val EntityRadius          = 1.0
  private val EntityDiameter        = EntityRadius * 2.0
  private val EntityWeight          = 2
  private val EntityHealth          = 100
  private val EntityDamage          = 1
  private val LinearSpeed           = 1.0
  private val AngularSpeed          = 30.0
  private val RotationStep          = 15.0
  private val RotationStepsPerTurn  = 24
  private val FullTurnRotation      = 0.0
  private val SurfaceFriction       = 0.1
  private val SurfaceForce          = Vector2D(0.25, 0.5)
  private val SurfaceDamage         = 1
  private val ExpectedEntitySpacing = 1.5
  private val ExpectedUpperLeft     = Vector2D(0.0, 0.0)
  private val ExpectedLowerRight    = Vector2D(4.0, 2.5)

  private val ExpectedSurfaceSize =
    Shape2D.rectangle(ExpectedLowerRight.y, ExpectedLowerRight.x).value

  private val ExpectedPositions = Set(
    Vector2D(0.5, 0.5),
    Vector2D(2.0, 0.5),
    Vector2D(3.5, 0.5),
    Vector2D(0.5, 2.0),
    Vector2D(2.0, 2.0),
    Vector2D(3.5, 2.0)
  )

  private val EntityCountValue = EntityCount.from(EntityNumber).value

  private def entityId(index: Int): String =
    s"$EntityIdPrefix-$index"

  private def entityAt(scene: Scene, index: Int): Entity =
    scene.allEntities.find(_.id.value == entityId(index)).value

  private def entityIds(scene: Scene): Set[String] =
    scene.allEntities.map(_.id.value).toSet

  private def sceneSnapshot(
      scene: Scene
  ): (Set[Entity], Set[Team], Set[Surface], Vector2D, Vector2D) =
    (
      scene.allEntities.toSet,
      scene.allTeams.toSet,
      scene.allSurfaces.toSet,
      scene.bounds.upperLeft,
      scene.bounds.lowerRight
    )

  test("a deterministic scene contains the requested number of entities"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.size) shouldBe Right(EntityNumber)

  test("a deterministic scene supports the minimum entity count"):
    val entityCount = EntityCount.from(MinimumEntityNumber).value

    val result = DeterministicScene(entityCount)

    result.map(_.allEntities.size) shouldBe Right(entityCount.value)

  test("a deterministic scene produces the same complete scene for the same entity count"):
    val first  = DeterministicScene(EntityCountValue)
    val second = DeterministicScene(EntityCountValue)

    first.map(sceneSnapshot) shouldBe second.map(sceneSnapshot)

  test("a deterministic scene assigns a stable identifier to every entity"):
    val expectedIds = (0 until EntityNumber).map(entityId).toSet

    val result = DeterministicScene(EntityCountValue)

    result.map(entityIds) shouldBe Right(expectedIds)

  test("a deterministic scene assigns a unique identifier to every entity"):
    val result = DeterministicScene(EntityCountValue)

    result.map(scene => entityIds(scene).size) shouldBe Right(EntityNumber)

  test("a deterministic scene creates circles at even indexes"):
    val expectedShape = Shape2D.circle(EntityRadius).value

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      (0 until EntityNumber by 2).map(index => entityAt(scene, index).shape).toSet
    } shouldBe Right(Set(expectedShape))

  test("a deterministic scene creates squares at odd indexes"):
    val expectedShape = Shape2D.rectangle(EntityDiameter, EntityDiameter).value

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      (1 until EntityNumber by 2).map(index => entityAt(scene, index).shape).toSet
    } shouldBe Right(Set(expectedShape))

  test("a deterministic scene gives every entity a positive weight"):
    val expectedWeight = Weight(EntityWeight).value

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.weight).toSet) shouldBe Right(Set(Some(expectedWeight)))

  test("a deterministic scene gives every entity positive health"):
    val expectedHealth = Health(EntityHealth).value

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.health).toSet) shouldBe Right(Set(Some(expectedHealth)))

  test("a deterministic scene gives every entity positive damage"):
    val expectedDamage = Damage(EntityDamage).value

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.damage).toSet) shouldBe Right(Set(Some(expectedDamage)))

  test("a deterministic scene gives every entity horizontal and vertical speed"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.forall(entity =>
        entity.speed.exists(speed => speed.x != 0.0 && speed.y != 0.0)
      )
    } shouldBe Right(true)
  test("a deterministic scene alternates horizontal movement direction"):
    val expectedSpeeds = Map(
      entityId(0) -> LinearSpeed,
      entityId(1) -> -LinearSpeed,
      entityId(2) -> LinearSpeed,
      entityId(3) -> -LinearSpeed,
      entityId(4) -> LinearSpeed,
      entityId(5) -> -LinearSpeed
    )

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.map(entity => entity.id.value -> entity.speed.value.x).toMap
    } shouldBe Right(expectedSpeeds)

  test("a deterministic scene alternates vertical movement direction"):
    val expectedSpeeds = Map(
      entityId(0) -> LinearSpeed,
      entityId(1) -> LinearSpeed,
      entityId(2) -> -LinearSpeed,
      entityId(3) -> -LinearSpeed,
      entityId(4) -> LinearSpeed,
      entityId(5) -> LinearSpeed
    )

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.map(entity => entity.id.value -> entity.speed.value.y).toMap
    } shouldBe Right(expectedSpeeds)

  test("a deterministic scene gives every entity an angular speed"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.forall(_.angularSpeed.exists(_ != 0.0))) shouldBe Right(true)

  test("a deterministic scene alternates angular movement direction"):
    val expectedAngularSpeeds = Map(
      entityId(0) -> AngularSpeed,
      entityId(1) -> -AngularSpeed,
      entityId(2) -> AngularSpeed,
      entityId(3) -> -AngularSpeed,
      entityId(4) -> AngularSpeed,
      entityId(5) -> -AngularSpeed
    )

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.map(entity => entity.id.value -> entity.angularSpeed.value).toMap
    } shouldBe Right(expectedAngularSpeeds)

  test("a deterministic scene assigns a stable rotation to every entity"):
    val expectedRotations = (0 until EntityNumber).map { index =>
      entityId(index) -> index * RotationStep
    }.toMap

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.map(entity => entity.id.value -> entity.rotation).toMap
    } shouldBe Right(expectedRotations)

  test("a deterministic scene wraps rotations after a full turn"):
    val entityCount = EntityCount.from(RotationStepsPerTurn + 1).value

    val result = DeterministicScene(entityCount)

    result.map(scene => entityAt(scene, RotationStepsPerTurn).rotation) shouldBe Right(
      FullTurnRotation
    )

  test("a deterministic scene arranges entities row by row on an overlapping grid"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allEntities.map(_.position).toSet) shouldBe Right(ExpectedPositions)

  test("a deterministic scene places adjacent entities close enough to collide"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      entityAt(scene, 0).position --> entityAt(scene, 1).position
    } shouldBe Right(ExpectedEntitySpacing)

  test("a deterministic scene derives tight bounds from its entity grid"):
    val expectedBounds = (ExpectedUpperLeft, ExpectedLowerRight)

    val result = DeterministicScene(EntityCountValue)

    result.map(scene => (scene.bounds.upperLeft, scene.bounds.lowerRight)) shouldBe Right(
      expectedBounds
    )

  test("a deterministic scene derives bounds for an incomplete final grid row"):
    val incompleteEntityCount = EntityNumber - 1
    val entityCount           = EntityCount.from(incompleteEntityCount).value
    val expectedBounds        = (ExpectedUpperLeft, ExpectedLowerRight)

    val result = DeterministicScene(entityCount)

    result.map(scene => (scene.bounds.upperLeft, scene.bounds.lowerRight)) shouldBe Right(
      expectedBounds
    )

  test("a deterministic scene crosses the left border"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.exists(_.position.x - EntityRadius < scene.bounds.upperLeft.x)
    } shouldBe Right(true)

  test("a deterministic scene crosses the right border"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.exists(_.position.x + EntityRadius > scene.bounds.lowerRight.x)
    } shouldBe Right(true)

  test("a deterministic scene crosses the top border"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.exists(_.position.y - EntityRadius < scene.bounds.upperLeft.y)
    } shouldBe Right(true)

  test("a deterministic scene crosses the bottom border"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.exists(_.position.y + EntityRadius > scene.bounds.lowerRight.y)
    } shouldBe Right(true)

  test("a deterministic scene contains two opposing teams"):
    val expectedTeams = Set(
      Team.create(FirstTeamId, Set(SecondTeamId)).value,
      Team.create(SecondTeamId, Set(FirstTeamId)).value
    )

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allTeams.toSet) shouldBe Right(expectedTeams)

  test("a deterministic scene alternates entity teams"):
    val expectedTeamIds = Map(
      entityId(0) -> FirstTeamId,
      entityId(1) -> SecondTeamId,
      entityId(2) -> FirstTeamId,
      entityId(3) -> SecondTeamId,
      entityId(4) -> FirstTeamId,
      entityId(5) -> SecondTeamId
    )

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.map(entity => entity.id.value -> entity.teamId.value.value).toMap
    } shouldBe Right(expectedTeamIds)

  test("a deterministic scene contains one full-scene surface"):
    val expectedPosition = Vector2D(
      ExpectedLowerRight.x / 2.0,
      ExpectedLowerRight.y / 2.0
    )

    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allSurfaces.map(surface => (surface.id.value, surface.position, surface.shape))
    } shouldBe Right(List((SurfaceId, expectedPosition, ExpectedSurfaceSize)))

  test("a deterministic scene surface applies friction"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allSurfaces.map(_.frictionIndex)) shouldBe Right(List(Some(SurfaceFriction)))

  test("a deterministic scene surface applies a force"):
    val result = DeterministicScene(EntityCountValue)

    result.map(_.allSurfaces.map(_.appliedForce)) shouldBe Right(List(Some(SurfaceForce)))

  test("a deterministic scene surface applies damage over time"):
    val expectedDamage = Damage(SurfaceDamage).value

    val result = DeterministicScene(EntityCountValue)

    result.map(_.allSurfaces.map(_.damageOverTime)) shouldBe Right(List(Some(expectedDamage)))

  test("a deterministic scene places every entity center inside its surface"):
    val result = DeterministicScene(EntityCountValue)

    result.map { scene =>
      scene.allEntities.forall { entity =>
        entity.position.x >= scene.bounds.upperLeft.x &&
        entity.position.y >= scene.bounds.upperLeft.y &&
        entity.position.x <= scene.bounds.lowerRight.x &&
        entity.position.y <= scene.bounds.lowerRight.y
      }
    } shouldBe Right(true)

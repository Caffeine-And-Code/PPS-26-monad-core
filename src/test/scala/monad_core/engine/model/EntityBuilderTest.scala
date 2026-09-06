package monad_core.engine.model

import monad_core.engine.model.EntityBuilder.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EntityBuilderTest extends AnyFunSuite with Inside with Matchers:

  val ValidEntityId           = "entity1"
  val ValidPosition: Vector2D = Vector2D(1, 3)
  val ValidRadius             = 2

  val ValidEntity: Either[EngineError, Entity] =
    Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

  test("can build an entity with all optional properties"):
    val speed        = Vector2D(3, 4)
    val angularSpeed = 45.0
    val weight       = 5
    val health       = 10
    val damage       = 2
    val teamId       = "team1"

    val entity = ValidEntity
      .withSpeed(Some(speed))
      .withAngularSpeed(Some(angularSpeed))
      .withWeight(Some(weight))
      .withHealth(Some(health))
      .withDamage(Some(damage))
      .withTeamId(Some(teamId))

    inside(entity):
      case Right(entity) =>
        entity.speed shouldBe Some(speed)
        entity.angularSpeed shouldBe Some(angularSpeed)
        entity.weight shouldBe Some(weight)
        entity.health shouldBe Some(health)
        entity.damage.map(_.value) shouldBe Some(damage)
        entity.teamId shouldBe Some(teamId)

  test("can build an entity without optional properties"):
    val entity = ValidEntity
      .withSpeed(None)
      .withAngularSpeed(None)
      .withWeight(None)
      .withHealth(None)
      .withDamage(None)
      .withTeamId(None)

    inside(entity):
      case Right(entity) =>
        entity.speed shouldBe None
        entity.angularSpeed shouldBe None
        entity.weight shouldBe None
        entity.health shouldBe None
        entity.damage shouldBe None
        entity.teamId shouldBe None

  test("cannot build an entity with an invalid weight"):
    val invalidWeight = -1

    val entity = ValidEntity.withWeight(Some(invalidWeight))

    entity shouldBe Left(WeightCannotBeNegativeOrZero())

  test("cannot build an entity with an invalid health"):
    val invalidHealth = 0

    val entity = ValidEntity.withHealth(Some(invalidHealth))

    entity shouldBe Left(HealthCannotBeNegativeOrZero(invalidHealth))

  test("cannot build an entity with an invalid damage"):
    val invalidDamage = -1

    val entity = ValidEntity.withDamage(Some(invalidDamage))

    entity shouldBe Left(DamageCannotBeNegative())

  test("cannot build an entity with an invalid team ID"):
    val invalidTeamId = "   "

    val entity = ValidEntity.withTeamId(Some(invalidTeamId))

    entity shouldBe Left(TeamIdCannotBeEmpty())

  test("an entity builder preserves a previous error"):
    val invalidEntity = Entity.circle("", ValidPosition, ValidRadius)

    val entity = invalidEntity.withHealth(Some(10))

    entity shouldBe Left(LocatableIdCannotBeEmpty())

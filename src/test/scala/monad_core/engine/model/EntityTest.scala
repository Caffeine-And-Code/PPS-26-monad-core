package monad_core.engine.model

import monad_core.engine.model.Shape2D.{Circle, Rectangle}
import monad_core.engine.model.{
  CannotApplyNegativeDamage,
  Entity,
  HealthCannotBeNegativeOrZero,
  LocatableIdCannotBeEmpty,
  PositionIsValid,
  Vector2D,
  WeightCannotBeNegativeOrZero
}
import org.scalatest.EitherValues.*
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*

class EntityTest extends AnyFunSuite with Inside with Matchers:

  val ValidEntityId           = "entity1"
  val ValidPosition: Vector2D = Vector2D(1, 3)
  val ValidRadius             = 2
  val ValidHeight             = 2
  val ValidLength             = 2

  val ValidEntity: Either[EngineError, Entity] =
    Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

  test("can create an entity with ID, position and the shape of a circle"):
    val entity = Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

    inside(entity):
      case Right(entity) =>
        entity.id.value shouldBe ValidEntityId
        entity.position shouldBe ValidPosition
        entity.shape shouldBe Shape2D.circle(ValidRadius).toOption.get
        entity.rotation shouldBe 0
        entity.speed shouldBe None
        entity.angularSpeed shouldBe None
        entity.weight shouldBe None
        entity.health shouldBe None
        entity.teamId shouldBe None

  test("can create an entity with ID, position and the shape of a rectangle"):
    val entity = Entity.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    inside(entity):
      case Right(entity) =>
        entity.id.value shouldBe ValidEntityId
        entity.position shouldBe ValidPosition
        entity.shape shouldBe Shape2D.rectangle(ValidHeight, ValidLength).toOption.get
        entity.speed shouldBe None
        entity.weight shouldBe None
        entity.health shouldBe None
        entity.teamId shouldBe None

  test("can create an entity in position 0,0"):
    val position00 = Vector2D(0, 0)
    val entity     = Entity.rectangle(ValidEntityId, position00, ValidHeight, ValidLength)

    inside(entity):
      case Right(entity) =>
        entity.position shouldBe position00

  test("cannot create an entity with an empty ID"):
    val invalidEntityId = "    "

    val invalidEntity = Entity.circle(invalidEntityId, ValidPosition, ValidRadius)

    invalidEntity shouldBe Left(LocatableIdCannotBeEmpty())

  test("cannot create an entity with an invalid position"):
    val invalidPositionX  = Vector2D(-1, 1)
    val invalidPositionY  = Vector2D(1, -1)
    val invalidPositionXY = Vector2D(-1, -1)

    val invalidForXPosition  = Entity.circle(ValidEntityId, invalidPositionX, ValidRadius)
    val invalidForYPosition  = Entity.circle(ValidEntityId, invalidPositionY, ValidRadius)
    val invalidForXYPosition = Entity.circle(ValidEntityId, invalidPositionXY, ValidRadius)

    invalidForXPosition shouldBe Left(PositionIsValid(invalidPositionX))
    invalidForYPosition shouldBe Left(PositionIsValid(invalidPositionY))
    invalidForXYPosition shouldBe Left(PositionIsValid(invalidPositionXY))

  test("can move entity in a given position"):
    val newPosition = Vector2D(4, 5)

    val entityInNewPosition = ValidEntity.map(_.moveTo(newPosition))

    inside(entityInNewPosition):
      case Right(entity) => entity.position shouldBe newPosition

  test("can move entity in an invalid position (necessary for bound collision resolution)"):
    val invalidPosition = Vector2D(-1, -1)

    val entityInNewPosition = ValidEntity.map(_.moveTo(invalidPosition))

    inside(entityInNewPosition):
      case Right(entity) => entity.position shouldBe invalidPosition

  test("can move an entity within a space"):
    val spaceVector = Vector2D(1, 3)

    val entityInNewPosition = ValidEntity.map(_.moveBy(spaceVector))

    inside(entityInNewPosition):
      case Right(entity) => entity.position shouldBe spaceVector + ValidPosition

  test("can create an entity and give it a speed"):
    val speed = Vector2D(3, 4)

    val entityWithSpeed = ValidEntity.map(_.withSpeed(speed))

    inside(entityWithSpeed):
      case Right(entity) => entity.speed shouldBe Some(speed)

  test("can create an entity and give it a weight"):
    val weight = 5

    val entityWithWeight = ValidEntity.flatMap(_.withWeight(weight))

    inside(entityWithWeight):
      case Right(entity) => entity.weight shouldBe Some(weight)

  test("cannot create an entity and give it an invalid weight"):
    val invalidWeight = -1

    val entityWithWeight = ValidEntity.flatMap(_.withWeight(invalidWeight))

    entityWithWeight shouldBe Left(WeightCannotBeNegativeOrZero())

  test("can create an entity and give it a health"):
    val health = 5

    val entityWithHealth = ValidEntity.flatMap(_.withHealth(health))

    inside(entityWithHealth):
      case Right(entity) => entity.health shouldBe Some(health)

  test("cannot create an entity and give it an invalid health"):
    val invalidHealth = -1

    val entityWithHealth = ValidEntity.flatMap(_.withHealth(invalidHealth))

    entityWithHealth shouldBe Left(HealthCannotBeNegativeOrZero(invalidHealth))

  test("can apply damage to an entity"):
    val health = 50
    val damage = 20

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    } yield entity

    inside(entity):
      case Right(entity) => entity.health shouldBe Some(health - damage)

  test("cannot apply damage to an entity without health"):
    val damage = 20

    val entity = ValidEntity.flatMap(_.applyDamage(damage))

    entity shouldBe Left(CannotApplyDamageToNoneHealthEntity())

  test("if apply a damage greater than life left, it returns Left"):
    val health = 50
    val damage = 60

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    } yield entity

    entity shouldBe Left(HealthCannotBeNegativeOrZero(health - damage))

  test("if apply a damage equals to life left, it returns Left"):
    val health = 50

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(health)
    } yield entity

    entity shouldBe Left(HealthCannotBeNegativeOrZero(0))

  test("cannot inflict a negative damage"):
    val health = 50
    val damage = -10

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    } yield entity

    entity shouldBe Left(CannotApplyNegativeDamage(damage))

  test("can add a teamId to an entity"):
    val teamId = "team1"

    val entityWithHealth = ValidEntity.flatMap(_.withTeamId(teamId))

    inside(entityWithHealth):
      case Right(entity) => entity.teamId shouldBe Some(teamId)

  test("check if entity is fixed"):

    inside(ValidEntity):
      case Right(entity) => entity.isFixed shouldBe true

  test("can remove speed from an entity"):
    val speed = Vector2D(3, 4)

    val withoutSpeedEntity = for {
      validEntity <- ValidEntity
      entity = validEntity.withSpeed(speed)
    } yield entity.withoutSpeed

    inside(withoutSpeedEntity):
      case Right(entity) => entity.isFixed shouldBe true

  test("can create and rotate an entity within the valid degree interval"):
    val rotations = Table(
      "rotation",
      0,
      90,
      360
    )

    forAll(rotations): rotation =>
      val entity = Entity.rectangle("entity", ValidPosition, 2, 4, rotation).value

      entity.rotation shouldBe rotation

  test("cannot create or rotate an entity outside the valid degree interval"):
    val entity = ValidEntity.value
    val negativeRotation = -1
    val excessiveRotation = 361

    val creationResult = Entity.circle("entity", ValidPosition, 2, negativeRotation)
    val rotationResult = entity.rotateTo(excessiveRotation)

    creationResult shouldBe Left(RotationMustBeAValidDegreeValue(negativeRotation))
    rotationResult shouldBe Left(RotationMustBeAValidDegreeValue(excessiveRotation))

  test("can add and remove angular speed from an entity"):
    val entity = ValidEntity.value
    val angularSpeed = -45

    val rotatingEntity = entity.withAngularSpeed(angularSpeed)
    val fixedEntity = rotatingEntity.withoutAngularSpeed

    rotatingEntity.angularSpeed shouldBe Some(angularSpeed)
    rotatingEntity.isFixed shouldBe false
    fixedEntity.angularSpeed shouldBe None

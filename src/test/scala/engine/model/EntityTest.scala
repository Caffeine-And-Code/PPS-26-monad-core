package engine.model

import engine.model.Shape2D.{Circle, Rectangle}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside

class EntityTest extends AnyFunSuite with Inside with Matchers:

  val ValidEntityId = "entity1"
  val ValidPosition = Vector2D(1, 3)
  val ValidRadius = 2
  val ValidHeight = 2
  val ValidLength = 2
  val ValidEntity: Either[String, Entity] = Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

  test("can create an entity with ID, position and the shape of a circle") :
    val entity = Entity.circle(ValidEntityId, ValidPosition, ValidRadius)

    inside(entity):
      case Right(entity) =>
        entity.id.value shouldBe ValidEntityId
        entity.position shouldBe ValidPosition
        entity.shape shouldBe Circle(ValidRadius)

  test("can create an entity with ID, position and the shape of a rectangle") :
    val entity = Entity.rectangle(ValidEntityId, ValidPosition, ValidHeight, ValidLength)

    inside(entity):
      case Right(entity) =>
        entity.id.value shouldBe ValidEntityId
        entity.position shouldBe ValidPosition
        entity.shape shouldBe Rectangle(ValidHeight, ValidLength)

  test("cannot create an entity with an empty ID"):
    val invalidEntityId = "    "

    val invalidEntity = Entity.circle(invalidEntityId, ValidPosition, ValidRadius)

    invalidEntity shouldBe Left("LocatableId cannot be empty")

  test("cannot create an entity with an invalid position"):
    val invalidPositionX = Vector2D(-1, 1)
    val invalidPositionY = Vector2D(1, -1)
    val invalidPositionXY = Vector2D(-1, -1)

    val invalidForXPosition = Entity.circle(ValidEntityId, invalidPositionX, ValidRadius)
    val invalidForYPosition = Entity.circle(ValidEntityId, invalidPositionY, ValidRadius)
    val invalidForXYPosition = Entity.circle(ValidEntityId, invalidPositionXY, ValidRadius)

    invalidForXPosition shouldBe Left("Position is invalid, x and y should be greater then 0")
    invalidForYPosition shouldBe Left("Position is invalid, x and y should be greater then 0")
    invalidForXYPosition shouldBe Left("Position is invalid, x and y should be greater then 0")

  test("can move entity in a given position"):
    val newPosition = Vector2D(4, 5)

    val entityInNewPosition = ValidEntity.flatMap(_.moveTo(newPosition))

    inside(entityInNewPosition) :
      case Right(entity) => entity.position shouldBe newPosition


  test("cannot move entity in an invalid position"):
    val invalidPosition = Vector2D(-1, -1)

    val entityInNewPosition = ValidEntity.flatMap(_.moveTo(invalidPosition))

    entityInNewPosition shouldBe Left("Position is invalid, x and y should be greater then 0")

  test("can move an entity within a space"):
    val spaceVector = Vector2D(1, 3)

    val entityInNewPosition = ValidEntity.flatMap(_.moveBy(spaceVector))

    inside(entityInNewPosition) :
      case Right(entity) => entity.position shouldBe spaceVector + ValidPosition


  test("can create an entity and give it a speed"):
    val speed = Vector2D(3, 4)

    val entityWithSpeed = ValidEntity.flatMap(_.withSpeed(speed))

    inside(entityWithSpeed) :
      case Right(entity) => entity.speed shouldBe Some(speed)


  test("can create an entity and give it a weight"):
    val weight = 5

    val entityWithWeight = ValidEntity.flatMap(_.withWeight(weight))

    inside(entityWithWeight):
      case Right(entity) => entity.weight shouldBe Some(weight)

  test("cannot create an entity and give it an invalid weight"):
    val invalidWeight = -1

    val entityWithWeight = ValidEntity.flatMap(_.withWeight(invalidWeight))

    entityWithWeight shouldBe Left("Weight cannot be negative")

  test("can create an entity and give it a health"):
    val health = 5

    val entityWithHealth = ValidEntity.flatMap(_.withHealth(health))

    inside(entityWithHealth):
      case Right(entity) => entity.health shouldBe Some(health)

  test("cannot create an entity and give it an invalid health"):
    val invalidHealth = -1

    val entityWithHealth = ValidEntity.flatMap(_.withHealth(invalidHealth))

    entityWithHealth shouldBe Left("Health cannot be negative or zero")

  test("can apply damage to an entity"):
    val health = 50
    val damage = 20

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    }yield entity

    inside(entity):
      case Right(entity) => entity.health shouldBe Some(health - damage)

  test("if apply a damage greater than life left, it returns Left"):
    val health = 50
    val damage = 60

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    } yield entity

    entity shouldBe Left("Health cannot be negative or zero")


  test("cannot inflict a negative damage"):
    val health = 50
    val damage = -10

    val entity = for {
      entity <- ValidEntity
      entity <- entity.withHealth(health)
      entity <- entity.applyDamage(damage)
    } yield entity

    entity shouldBe Left("Cannot apply a negative damage")
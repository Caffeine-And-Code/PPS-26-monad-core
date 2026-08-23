package monad_core.engine.physics.utils

import monad_core.engine.core.{
  CannotAddAlreadyPresentElementInMap,
  CannotAddEntity,
  CannotRemoveEntity,
  CannotRemoveNonPresentElementFromMap
}
import monad_core.engine.model.Vector2D
import monad_core.engine.physics.core.PhysicsDomainError
import monad_core.engine.helper.DummyEntityHelper.makeMovingEntityCircle
import monad_core.engine.helper.MockStateHelper
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SceneEntitiesUpdateTest
    extends AnyFunSuite
    with Matchers
    with MockFactory
    with MockStateHelper:

  private val Entity1 = makeMovingEntityCircle(
    id = "entity1",
    position = Vector2D(10, 10),
    speed = Vector2D(1, 1)
  )

  private val Entity2 = makeMovingEntityCircle(
    id = "entity2",
    position = Vector2D(20, 20),
    speed = Vector2D(2, 2)
  )

  test("this function should return the same scene if the list of updated entities is empty"):
    val initialScene = stateWithEntities(List(Entity1))

    val result = SceneEntitiesUpdate(initialScene, List()).value

    val resultEntity = result.allEntities.find(_.id == Entity1.id).value

    result.allEntities.size shouldBe 1
    resultEntity.id shouldBe Entity1.id
    resultEntity.position shouldBe Entity1.position
    resultEntity.speed shouldBe Entity1.speed

  test("this function should update entities in the scene"):

    val updatedEntity = Entity1.moveTo(Vector2D(11, 11)).withSpeed(Vector2D(2, 2))

    val initialScene = stateWithEntities(List(Entity1))

    val result = SceneEntitiesUpdate(initialScene, List(updatedEntity)).value

    val resultEntity = result.allEntities.find(_.id == updatedEntity.id).value

    resultEntity.id shouldBe Entity1.id
    resultEntity.position shouldBe updatedEntity.position
    resultEntity.speed shouldBe updatedEntity.speed

  test("this function should only update entities that are in the updated list"):

    val updatedEntity = Entity1.moveTo(Vector2D(11, 11)).withSpeed(Vector2D(2, 2))

    val initialScene = stateWithEntities(List(Entity1, Entity2))

    val result = SceneEntitiesUpdate(initialScene, List(updatedEntity)).value

    val resultEntity1 = result.allEntities.find(_.id == updatedEntity.id).value
    val resultEntity2 = result.allEntities.find(_.id == Entity2.id).value

    resultEntity1.id shouldBe Entity1.id
    resultEntity1.position shouldBe updatedEntity.position
    resultEntity1.speed shouldBe updatedEntity.speed

    resultEntity2.id shouldBe Entity2.id
    resultEntity2.position shouldBe Entity2.position
    resultEntity2.speed shouldBe Entity2.speed

  test("this function should update multiple entities that are in the updated list"):

    val updatedEntity1 = Entity1.moveTo(Vector2D(11, 11)).withSpeed(Vector2D(2, 2))
    val updatedEntity2 = Entity2.moveTo(Vector2D(21, 21)).withSpeed(Vector2D(3, 3))

    val initialScene = stateWithEntities(List(Entity1, Entity2))

    val result = SceneEntitiesUpdate(initialScene, List(updatedEntity1, updatedEntity2)).value

    val resultEntity1 = result.allEntities.find(_.id == updatedEntity1.id).value
    val resultEntity2 = result.allEntities.find(_.id == updatedEntity2.id).value

    resultEntity1.id shouldBe Entity1.id
    resultEntity1.position shouldBe updatedEntity1.position
    resultEntity1.speed shouldBe updatedEntity1.speed

    resultEntity2.id shouldBe Entity2.id
    resultEntity2.position shouldBe updatedEntity2.position
    resultEntity2.speed shouldBe updatedEntity2.speed

  test("this function should return an error if an entity in the updated list is not in the scene"):

    val updatedEntity = Entity1.moveTo(Vector2D(11, 11)).withSpeed(Vector2D(2, 2))

    val initialScene = stateWithEntities(List(Entity2))

    val result = SceneEntitiesUpdate(initialScene, List(updatedEntity))

    result shouldBe Left(
      PhysicsDomainError(
        CannotRemoveEntity(
          CannotRemoveNonPresentElementFromMap(Entity1.id)
        )
      )
    )

  test(
    "this function should return an error if it tries to add an entity that is already in the scene"
  ):

    val updatedEntity = Entity1.moveTo(Vector2D(11, 11)).withSpeed(Vector2D(2, 2))

    val initialScene = stateWithEntitiesNotRemoving(List(Entity1))

    val result = SceneEntitiesUpdate(initialScene, List(updatedEntity))

    result shouldBe Left(
      PhysicsDomainError(
        CannotAddEntity(
          CannotAddAlreadyPresentElementInMap(Entity1.id)
        )
      )
    )

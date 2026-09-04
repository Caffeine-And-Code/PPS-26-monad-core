package integrations.monad_core.performance.simulator

import monad_core.engine.model.*
import monad_core.performance.model.EntityCount
import monad_core.performance.simulator.DeterministicScene
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DeterministicSceneTest extends AnyFunSuite with Matchers:

  private def scene(entityCount: Int): Scene =
    DeterministicScene(EntityCount.from(entityCount).value).value

  private def entityAt(index: Int, entityCount: Int = 4): Entity =
    scene(entityCount).allEntities.find(_.id.value == s"performance-entity-$index").value

  private def surface(entityCount: Int = 4): Surface =
    scene(entityCount).allSurfaces.head

  test("the scene contains the requested number of entities"):
    val result = scene(5)

    result.allEntities should have size 5

  test("the scene contains two teams"):
    val result = scene(1)

    result.allTeams should have size 2

  test("the first team considers the second team an enemy"):
    val result = scene(1)
    val first  = result.allTeams.find(_.id.value == "performance-team-a").value

    val resultValue = first.enemies.map(_.value)

    resultValue should contain("performance-team-b")

  test("the second team considers the first team an enemy"):
    val result = scene(1)
    val second = result.allTeams.find(_.id.value == "performance-team-b").value

    val resultValue = second.enemies.map(_.value)

    resultValue should contain("performance-team-a")

  test("the scene contains one surface"):
    val result = scene(1)

    result.allSurfaces should have size 1

  test("the surface has the deterministic identifier"):
    val result = surface()

    result.id.value shouldBe "performance-surface"

  test("the surface has deterministic friction"):
    val result = surface()

    result.frictionIndex.value shouldBe 0.1

  test("the surface has a deterministic applied force"):
    val result = surface()

    result.appliedForce.value shouldBe Vector2D(0.25, 0.5)

  test("the surface has deterministic damage"):
    val result = surface()

    result.damageOverTime.value.value shouldBe 1

  test("even entities are circles"):
    val result = entityAt(0)

    result.shape shouldBe a[Shape2D.Circle]

  test("circle entities have deterministic radius"):
    val result = entityAt(0).shape.asInstanceOf[Shape2D.Circle]

    result.radius shouldBe 1.0

  test("odd entities are rectangles"):
    val result = entityAt(1)

    result.shape shouldBe a[Shape2D.Rectangle]

  test("rectangle entities have deterministic height"):
    val result = entityAt(1).shape.asInstanceOf[Shape2D.Rectangle]

    result.height shouldBe 2.0

  test("rectangle entities have deterministic length"):
    val result = entityAt(1).shape.asInstanceOf[Shape2D.Rectangle]

    result.length shouldBe 2.0

  test("entities have deterministic identifiers"):
    val result = entityAt(3)

    result.id.value shouldBe "performance-entity-3"

  test("entity identifiers are unique"):
    val result = scene(5).allEntities.map(_.id)

    result.distinct should have size 5

  test("the first entity has deterministic horizontal position"):
    val result = entityAt(0)

    result.position.x shouldBe 0.5

  test("the first entity has deterministic vertical position"):
    val result = entityAt(0)

    result.position.y shouldBe 0.5

  test("entities advance horizontally by deterministic spacing"):
    val result = entityAt(1)

    result.position.x shouldBe 2.0

  test("entities wrap onto the next deterministic row"):
    val result = entityAt(2)

    result.position shouldBe Vector2D(0.5, 2.0)

  test("entities have deterministic weight"):
    val result = entityAt(0)

    result.weight.value.value shouldBe 2

  test("entities have deterministic health"):
    val result = entityAt(0)

    result.health.value.value shouldBe 100

  test("entities have deterministic contact damage"):
    val result = entityAt(0)

    result.damage.value.value shouldBe 1

  test("even entities belong to the first team"):
    val result = entityAt(0)

    result.teamId.value.value shouldBe "performance-team-a"

  test("odd entities belong to the second team"):
    val result = entityAt(1)

    result.teamId.value.value shouldBe "performance-team-b"

  test("the first entity has positive horizontal speed"):
    val result = entityAt(0)

    result.speed.value.x shouldBe 1.0

  test("the second entity has negative horizontal speed"):
    val result = entityAt(1)

    result.speed.value.x shouldBe -1.0

  test("the first vertical direction block moves downwards"):
    val result = entityAt(0)

    result.speed.value.y shouldBe 1.0

  test("the second vertical direction block moves upwards"):
    val result = entityAt(2)

    result.speed.value.y shouldBe -1.0

  test("even entities have positive angular speed"):
    val result = entityAt(0)

    result.angularSpeed.value shouldBe 30.0

  test("odd entities have negative angular speed"):
    val result = entityAt(1)

    result.angularSpeed.value shouldBe -30.0

  test("entity rotation advances by a deterministic step"):
    val result = entityAt(1)

    result.rotation shouldBe 15.0

  test("entity rotation wraps after one full turn"):
    val result = entityAt(24, entityCount = 25)

    result.rotation shouldBe 0.0

  test("the generated world bounds are positive"):
    val result = scene(1)

    result.bounds.lowerRight.x should be > 0.0

  test("world width follows the deterministic entity grid"):
    val result = scene(4)

    result.bounds.lowerRight.x shouldBe 2.5

  test("world height follows the deterministic entity grid"):
    val result = scene(4)

    result.bounds.lowerRight.y shouldBe 2.5

  test("the surface is horizontally centered in the world"):
    val result = surface()

    result.position.x shouldBe 1.25

  test("the surface is vertically centered in the world"):
    val result = surface()

    result.position.y shouldBe 1.25

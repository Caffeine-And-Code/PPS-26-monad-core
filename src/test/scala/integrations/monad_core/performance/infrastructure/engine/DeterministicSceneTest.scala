package integrations.monad_core.performance.infrastructure.engine

import monad_core.engine.model.Scene
import monad_core.engine.physics.core.PhysicsManager
import monad_core.engine.simulator.EngineFacade
import monad_core.performance.domain.EntityCount
import monad_core.performance.infrastructure.engine.DeterministicScene
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DeterministicSceneTest extends AnyFunSuite with Matchers:

  private val EntityNumber = 6
  private val EntityHealth = 100

  private val EntityCountValue = EntityCount.from(EntityNumber).value

  private def applyDefaultPhysics(scene: Scene) =
    PhysicsManager.default().step(scene, EngineFacade.DefaultTickTime)

  test("default physics can process the complete deterministic scene"):
    val scene = DeterministicScene(EntityCountValue).value

    val result = applyDefaultPhysics(scene)

    result.isRight shouldBe true

  test("default physics applies damage in the complete deterministic scene"):
    val scene = DeterministicScene(EntityCountValue).value

    val result = applyDefaultPhysics(scene)

    result.map(_.state.allEntities.forall(_.health.exists(_.value < EntityHealth))) shouldBe Right(
      true
    )

  test("default physics keeps every deterministic entity alive after applying damage"):
    val scene = DeterministicScene(EntityCountValue).value

    val result = applyDefaultPhysics(scene)

    result.map(_.state.allEntities.size) shouldBe Right(EntityNumber)

  test("every subset of default physics rules can process the deterministic scene"):
    val scene          = DeterministicScene(EntityCountValue).value
    val defaultManager = PhysicsManager.default()
    val rules          = defaultManager.rules.toVector
    val subsetCount    = 1 << rules.size

    val results = (0 until subsetCount).map { mask =>
      val selectedManager = rules.zipWithIndex.foldLeft(defaultManager.disableAll) {
        case (manager, (rule, index)) =>
          if (mask & (1 << index)) != 0 then manager.enable(rule) else manager
      }

      selectedManager.step(scene, EngineFacade.DefaultTickTime)
    }

    results.forall(_.isRight) shouldBe true

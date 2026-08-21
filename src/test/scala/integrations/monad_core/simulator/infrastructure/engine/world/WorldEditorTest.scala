package integrations.monad_core.simulator.infrastructure.engine.world

import monad_core.engine.core.LoopMode
import monad_core.engine.core.events.EngineEvent.{EntityCreated, EntityRemoved, EntityUpdated}
import monad_core.engine.model.{Entity, Scene, Surface, Team, Vector2D}
import monad_core.simulator.infrastructure.engine.world.WorldEdit.*
import monad_core.simulator.infrastructure.engine.world.{SceneEditingNotAllowed, WorldEditor}
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldEditorTest extends AnyFunSuite with Matchers:

  private val entity         = Entity.circle("entity", Vector2D(0, 0), 1).value
  private val updatedEntity  = entity.moveTo(Vector2D(1, 1))
  private val surface        = Surface.circle("surface", Vector2D(0, 0), 1).value
  private val updatedSurface = Surface.circle("surface", Vector2D(1, 1), 2).value
  private val team           = Team.create("team").value
  private val updatedTeam    = Team.create("team", Set("enemy")).value

  test("simulation mode rejects an edit before changing a real scene"):
    val initialScene = Scene()

    val result = WorldEditor(LoopMode.SimulationMode, initialScene, CreateEntity(entity))

    result shouldBe Left(SceneEditingNotAllowed)
    initialScene.entities shouldBe empty

  test("entity creation integrates with the scene model"):
    val initialScene = Scene()

    val result = WorldEditor(LoopMode.EditMode, initialScene, CreateEntity(entity)).value

    result.scene.getEntity(entity.id).value shouldBe entity
    result.events shouldBe Vector(EntityCreated(entity))

  test("entity update integrates with the scene model"):
    val initialScene = Scene().addEntity(entity).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, UpdateEntity(updatedEntity)).value

    result.scene.getEntity(entity.id).value shouldBe updatedEntity
    result.events shouldBe Vector(EntityUpdated(entity, updatedEntity))

  test("entity removal integrates with the scene model"):
    val initialScene = Scene().addEntity(entity).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, RemoveEntity(entity.id)).value

    result.scene.entities shouldBe empty
    result.events shouldBe Vector(EntityRemoved(entity))

  test("surface creation integrates with the scene model"):
    val initialScene = Scene()

    val result = WorldEditor(LoopMode.EditMode, initialScene, CreateSurface(surface)).value

    result.scene.getSurface(surface.id).value shouldBe surface

  test("surface update integrates with the scene model"):
    val initialScene = Scene().addSurface(surface).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, UpdateSurface(updatedSurface)).value

    result.scene.getSurface(surface.id).value shouldBe updatedSurface

  test("surface removal integrates with the scene model"):
    val initialScene = Scene().addSurface(surface).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, RemoveSurface(surface.id)).value

    result.scene.surfaces shouldBe empty

  test("team creation integrates with the scene model"):
    val initialScene = Scene()

    val result = WorldEditor(LoopMode.EditMode, initialScene, CreateTeam(team)).value

    result.scene.getTeam(team.id).value shouldBe team

  test("team update integrates with the scene model"):
    val initialScene = Scene().addTeam(team).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, UpdateTeam(updatedTeam)).value

    result.scene.getTeam(team.id).value shouldBe updatedTeam

  test("team removal integrates with the scene model"):
    val initialScene = Scene().addTeam(team).value

    val result = WorldEditor(LoopMode.EditMode, initialScene, RemoveTeam(team.id)).value

    result.scene.teams shouldBe empty

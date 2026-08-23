package monad_core.simulator.infrastructure.engine.world

import monad_core.engine.core.*
import monad_core.engine.core.events.EngineEvent.{EntityCreated, EntityRemoved, EntityUpdated}
import monad_core.engine.model.*
import monad_core.simulator.application.engine.errors.EngineErrorAdapted
import monad_core.simulator.infrastructure.engine.world.WorldEdit.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues.convertEitherToValuable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WorldEditorTest extends AnyFunSuite with Matchers with MockFactory:

  private val entity         = Entity.circle("entity", Vector2D(0, 0), 1).value
  private val updatedEntity  = entity.moveTo(Vector2D(1, 1))
  private val surface        = Surface.circle("surface", Vector2D(0, 0), 1).value
  private val updatedSurface = Surface.circle("surface", Vector2D(1, 1), 2).value
  private val team           = Team.create("team").value
  private val updatedTeam    = Team.create("team", Set("enemy")).value

  test("an edit is rejected in simulation mode"):
    val scene = mock[Scene]

    val result = WorldEditor(LoopMode.SimulationMode, scene, CreateEntity(entity))

    result shouldBe Left(SceneEditingNotAllowed)

  test("creating an entity returns the updated scene and a creation event"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.addEntity.expects(entity).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateEntity(entity))

    result shouldBe Right(WorldEditResult(updatedScene, Vector(EntityCreated(entity))))

  test("an entity creation error is adapted"):
    val scene = mock[Scene]
    val error = CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id))
    scene.addEntity.expects(entity).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateEntity(entity))

    result shouldBe Left(EngineErrorAdapted(error))

  test("updating an entity atomically replaces it and returns an update event"):
    val scene        = mock[Scene]
    val withoutOld   = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getEntity.expects(entity.id).returning(Right(entity))
    scene.removeEntity.expects(entity).returning(Right(withoutOld))
    withoutOld.addEntity.expects(updatedEntity).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateEntity(updatedEntity))

    result shouldBe Right(
      WorldEditResult(updatedScene, Vector(EntityUpdated(entity, updatedEntity)))
    )

  test("an entity update stops when the entity lookup fails"):
    val scene = mock[Scene]
    val error = EntityNotFound(entity.id)
    scene.getEntity.expects(entity.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateEntity(updatedEntity))

    result shouldBe Left(EngineErrorAdapted(error))

  test("an entity update stops when removal fails"):
    val scene = mock[Scene]
    val error = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id))
    scene.getEntity.expects(entity.id).returning(Right(entity))
    scene.removeEntity.expects(entity).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateEntity(updatedEntity))

    result shouldBe Left(EngineErrorAdapted(error))

  test("an entity update stops when replacement fails"):
    val scene      = mock[Scene]
    val withoutOld = mock[Scene]
    val error      = CannotAddEntity(CannotAddAlreadyPresentElementInMap(entity.id))
    scene.getEntity.expects(entity.id).returning(Right(entity))
    scene.removeEntity.expects(entity).returning(Right(withoutOld))
    withoutOld.addEntity.expects(updatedEntity).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateEntity(updatedEntity))

    result shouldBe Left(EngineErrorAdapted(error))

  test("removing an entity returns the updated scene and a removal event"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getEntity.expects(entity.id).returning(Right(entity))
    scene.removeEntity.expects(entity).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveEntity(entity.id))

    result shouldBe Right(WorldEditResult(updatedScene, Vector(EntityRemoved(entity))))

  test("an entity removal stops when the entity lookup fails"):
    val scene = mock[Scene]
    val error = EntityNotFound(entity.id)
    scene.getEntity.expects(entity.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveEntity(entity.id))

    result shouldBe Left(EngineErrorAdapted(error))

  test("an entity removal error is adapted"):
    val scene = mock[Scene]
    val error = CannotRemoveEntity(CannotRemoveNonPresentElementFromMap(entity.id))
    scene.getEntity.expects(entity.id).returning(Right(entity))
    scene.removeEntity.expects(entity).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveEntity(entity.id))

    result shouldBe Left(EngineErrorAdapted(error))

  test("creating a surface returns the updated scene without events"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.addSurface.expects(surface).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateSurface(surface))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a surface creation error is adapted"):
    val scene = mock[Scene]
    val error = CannotAddSurface(CannotAddAlreadyPresentElementInMap(surface.id))
    scene.addSurface.expects(surface).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateSurface(surface))

    result shouldBe Left(EngineErrorAdapted(error))

  test("updating a surface atomically replaces it without events"):
    val scene        = mock[Scene]
    val withoutOld   = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getSurface.expects(surface.id).returning(Right(surface))
    scene.removeSurface.expects(surface).returning(Right(withoutOld))
    withoutOld.addSurface.expects(updatedSurface).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateSurface(updatedSurface))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a surface update stops when the surface lookup fails"):
    val scene = mock[Scene]
    val error = SurfaceNotFound(surface.id)
    scene.getSurface.expects(surface.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateSurface(updatedSurface))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a surface update stops when removal fails"):
    val scene = mock[Scene]
    val error = CannotRemoveSurface(CannotRemoveNonPresentElementFromMap(surface.id))
    scene.getSurface.expects(surface.id).returning(Right(surface))
    scene.removeSurface.expects(surface).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateSurface(updatedSurface))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a surface update stops when replacement fails"):
    val scene      = mock[Scene]
    val withoutOld = mock[Scene]
    val error      = CannotAddSurface(CannotAddAlreadyPresentElementInMap(surface.id))
    scene.getSurface.expects(surface.id).returning(Right(surface))
    scene.removeSurface.expects(surface).returning(Right(withoutOld))
    withoutOld.addSurface.expects(updatedSurface).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateSurface(updatedSurface))

    result shouldBe Left(EngineErrorAdapted(error))

  test("removing a surface returns the updated scene without events"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getSurface.expects(surface.id).returning(Right(surface))
    scene.removeSurface.expects(surface).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveSurface(surface.id))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a surface removal stops when the surface lookup fails"):
    val scene = mock[Scene]
    val error = SurfaceNotFound(surface.id)
    scene.getSurface.expects(surface.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveSurface(surface.id))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a surface removal error is adapted"):
    val scene = mock[Scene]
    val error = CannotRemoveSurface(CannotRemoveNonPresentElementFromMap(surface.id))
    scene.getSurface.expects(surface.id).returning(Right(surface))
    scene.removeSurface.expects(surface).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveSurface(surface.id))

    result shouldBe Left(EngineErrorAdapted(error))

  test("creating a team returns the updated scene without events"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.addTeam.expects(team).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateTeam(team))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a team creation error is adapted"):
    val scene = mock[Scene]
    val error = CannotAddTeam(CannotAddAlreadyPresentElementInMap(team.id))
    scene.addTeam.expects(team).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, CreateTeam(team))

    result shouldBe Left(EngineErrorAdapted(error))

  test("updating a team atomically replaces it without events"):
    val scene        = mock[Scene]
    val withoutOld   = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getTeam.expects(team.id).returning(Right(team))
    scene.removeTeam.expects(team).returning(Right(withoutOld))
    withoutOld.addTeam.expects(updatedTeam).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateTeam(updatedTeam))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a team update stops when the team lookup fails"):
    val scene = mock[Scene]
    val error = TeamNotFound(team.id)
    scene.getTeam.expects(team.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateTeam(updatedTeam))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a team update stops when removal fails"):
    val scene = mock[Scene]
    val error = CannotRemoveTeam(CannotRemoveNonPresentElementFromMap(team.id))
    scene.getTeam.expects(team.id).returning(Right(team))
    scene.removeTeam.expects(team).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateTeam(updatedTeam))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a team update stops when replacement fails"):
    val scene      = mock[Scene]
    val withoutOld = mock[Scene]
    val error      = CannotAddTeam(CannotAddAlreadyPresentElementInMap(team.id))
    scene.getTeam.expects(team.id).returning(Right(team))
    scene.removeTeam.expects(team).returning(Right(withoutOld))
    withoutOld.addTeam.expects(updatedTeam).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, UpdateTeam(updatedTeam))

    result shouldBe Left(EngineErrorAdapted(error))

  test("removing a team returns the updated scene without events"):
    val scene        = mock[Scene]
    val updatedScene = mock[Scene]
    scene.getTeam.expects(team.id).returning(Right(team))
    scene.removeTeam.expects(team).returning(Right(updatedScene))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveTeam(team.id))

    result shouldBe Right(WorldEditResult(updatedScene))

  test("a team removal stops when the team lookup fails"):
    val scene = mock[Scene]
    val error = TeamNotFound(team.id)
    scene.getTeam.expects(team.id).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveTeam(team.id))

    result shouldBe Left(EngineErrorAdapted(error))

  test("a team removal error is adapted"):
    val scene = mock[Scene]
    val error = CannotRemoveTeam(CannotRemoveNonPresentElementFromMap(team.id))
    scene.getTeam.expects(team.id).returning(Right(team))
    scene.removeTeam.expects(team).returning(Left(error))

    val result = WorldEditor(LoopMode.EditMode, scene, RemoveTeam(team.id))

    result shouldBe Left(EngineErrorAdapted(error))

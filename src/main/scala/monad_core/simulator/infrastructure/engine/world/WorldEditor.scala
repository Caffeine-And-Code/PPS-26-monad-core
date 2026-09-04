package monad_core.simulator.infrastructure.engine.world

import monad_core.engine.core.LoopMode
import monad_core.engine.core.events.EngineEvent.{EntityCreated, EntityRemoved, EntityUpdated}
import monad_core.engine.model.Scene
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.engine.world.WorldEdit.*

/**
 * Pure interpreter for [[WorldEdit]] commands.
 *
 * Edits are accepted only in `LoopMode.EditMode`. Successful operations return a new
 * immutable scene and any engine events produced by the mutation; the input scene is never
 * modified in place.
 */
object WorldEditor:

  /**
   * Applies one edit command to a scene when the engine is in edit mode.
   *
   * Entity creation, update, and removal emit the corresponding lifecycle event. Surface
   * and team operations update the scene without emitting events.
   *
   * @param mode current engine loop mode
   * @param scene scene on which the command is interpreted
   * @param edit mutation to apply
   * @return `Right(WorldEditResult)` containing the updated scene on success;
   *         `Left(SceneEditingNotAllowed)` in simulation mode, or another `Left(BaseError)`
   *         when domain validation fails
   */
  def apply(
      mode: LoopMode,
      scene: Scene,
      edit: WorldEdit
  ): Either[BaseError, WorldEditResult] =
    mode match
      case LoopMode.SimulationMode => Left(SceneEditingNotAllowed)
      case LoopMode.EditMode       => interpret(scene, edit)

  private def interpret(scene: Scene, edit: WorldEdit): Either[BaseError, WorldEditResult] =
    edit match
      case CreateEntity(entity) =>
        scene
          .addEntity(entity)
          .adaptError()
          .map(WorldEditResult(_, Vector(EntityCreated(entity))))

      case UpdateEntity(entity) =>
        for
          previous     <- scene.getEntity(entity.id).adaptError()
          withoutOld   <- scene.removeEntity(previous).adaptError()
          updatedScene <- withoutOld.addEntity(entity).adaptError()
        yield WorldEditResult(updatedScene, Vector(EntityUpdated(previous, entity)))

      case RemoveEntity(id) =>
        for
          entity       <- scene.getEntity(id).adaptError()
          updatedScene <- scene.removeEntity(entity).adaptError()
        yield WorldEditResult(updatedScene, Vector(EntityRemoved(entity)))

      case CreateSurface(surface) =>
        scene.addSurface(surface).adaptError().map(WorldEditResult(_))

      case UpdateSurface(surface) =>
        for
          previous     <- scene.getSurface(surface.id).adaptError()
          withoutOld   <- scene.removeSurface(previous).adaptError()
          updatedScene <- withoutOld.addSurface(surface).adaptError()
        yield WorldEditResult(updatedScene)

      case RemoveSurface(id) =>
        for
          surface      <- scene.getSurface(id).adaptError()
          updatedScene <- scene.removeSurface(surface).adaptError()
        yield WorldEditResult(updatedScene)

      case CreateTeam(team) =>
        scene.addTeam(team).adaptError().map(WorldEditResult(_))

      case UpdateTeam(team) =>
        for
          previous     <- scene.getTeam(team.id).adaptError()
          withoutOld   <- scene.removeTeam(previous).adaptError()
          updatedScene <- withoutOld.addTeam(team).adaptError()
        yield WorldEditResult(updatedScene)

      case RemoveTeam(id) =>
        for
          team         <- scene.getTeam(id).adaptError()
          updatedScene <- scene.removeTeam(team).adaptError()
        yield WorldEditResult(updatedScene)

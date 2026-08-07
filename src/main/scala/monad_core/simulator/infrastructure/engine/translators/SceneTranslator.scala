package monad_core.simulator.infrastructure.engine.translators

import monad_core.engine.core.Scene
import monad_core.engine.errors.EngineError
import monad_core.simulator.domain.engine.MonadCoreScene
import monad_core.simulator.infrastructure.engine.translators.EntityTranslator.{toSimulationEntity, toEngineModel as toEngineEntity}
import monad_core.simulator.infrastructure.engine.translators.SurfaceTranslator.{toSimulationSurface, toEngineModel as toEngineSurface}
import monad_core.simulator.infrastructure.engine.translators.TeamTranslator.{toSimulationTeam, toEngineModel as toEngineTeam}

object SceneTranslator:

  private def sequence[A](eithers: List[Either[EngineError, A]]): Either[EngineError, List[A]] =
    val (errors, values) = eithers.partitionMap(identity)
    errors match
      case firstError :: _ => Left(firstError)
      case Nil             => Right(values)

  extension (scene: Scene)
    def toSimulationScene: MonadCoreScene =
      MonadCoreScene(
        entities = scene.allEntities.map(_.toSimulationEntity),
        teams = scene.allTeams.map(_.toSimulationTeam),
        surfaces = scene.allSurfaces.map(_.toSimulationSurface)
      )

  extension (simulationScene: MonadCoreScene)
    def toEngineModel: Either[EngineError, Scene] =
      for
        entities <- sequence(simulationScene.entities.map(_.toEngineEntity))
        teams <- sequence(simulationScene.teams.map(_.toEngineTeam))
        surfaces <- sequence(simulationScene.surfaces.map(_.toEngineSurface))
      yield Scene(
        entities = entities.map(e => e.id -> e).toMap,
        teams = teams.map(t => t.id -> t).toMap,
        surfaces = surfaces.map(s => s.id -> s).toMap
      )
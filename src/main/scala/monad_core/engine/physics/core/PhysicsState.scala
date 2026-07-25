package monad_core.engine.physics.core

import monad_core.engine.model.{Entity, LocatableId, Surface, Team, TeamId}

trait PhysicsState[S]:
  def getEntities(scene: S): Map[LocatableId, Entity]
  def getSurfaces(scene: S): Map[LocatableId, Surface]
  def updateEntity(scene: S, id: LocatableId, entity: Entity): S
  def getTeams(scene: S): Map[TeamId, Team]
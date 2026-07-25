package engine.physics.core

import engine.model.{Entity, LocatableId}

trait PhysicsState[S]:
  def getEntities(scene: S): Map[LocatableId, Entity]
  def updateEntity(scene: S, id: LocatableId, entity: Entity): S
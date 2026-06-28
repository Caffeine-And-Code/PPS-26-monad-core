package engine.core.traits

import engine.model.{Entity, Surface, LocatableId}

trait Scene {
  def entities: Map[LocatableId, Entity]
  def surfaces: Map[LocatableId, Surface]

  def withEntities(updatedEntities: Map[LocatableId, Entity]): Scene
}
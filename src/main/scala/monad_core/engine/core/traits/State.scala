package monad_core.engine.core.traits

import monad_core.engine.core.Scene
import monad_core.engine.errors.EngineError
import monad_core.engine.model.*

trait State:
  def allEntities : List[Entity]
  
  def allTeams: List[Team]
  
  def allSurfaces: List[Surface]

  def getEntity(id: LocatableId): Either[EngineError, Entity]

  def getTeam(id: TeamId): Either[EngineError, Team]

  def getSurface(id: LocatableId): Either[EngineError, Surface]

  def addEntity(entity: Entity): Either[EngineError, Scene]

  def addTeam(team: Team): Either[EngineError, Scene]

  def addSurface(surface: Surface): Either[EngineError, Scene]

  def removeEntity(entity: Entity): Either[EngineError, Scene]

  def removeTeam(team: Team): Either[EngineError, Scene]

  def removeSurface(surface: Surface): Either[EngineError, Scene]

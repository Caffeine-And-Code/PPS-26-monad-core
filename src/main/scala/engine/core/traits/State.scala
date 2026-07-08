package engine.core.traits

import engine.errors.EngineError
import engine.model.*

trait State[S]:
  def getEntity(state: S, id: LocatableId): Either[EngineError, Entity]
  def getTeam(state: S, id: TeamId): Either[EngineError, Team]
  def getSurface(state: S, id: LocatableId): Either[EngineError, Surface]

  def addEntity(state: S, entity: Entity): Either[EngineError, S]
  def addTeam(state: S, team: Team): Either[EngineError, S]
  def addSurface(state: S, surface: Surface): Either[EngineError, S]

  def removeEntity(state: S, entity: Entity): Either[EngineError, S]
  def removeTeam(state: S, team: Team): Either[EngineError, S]
  def removeSurface(state: S, surface: Surface): Either[EngineError, S]
  
  def updateEntity(state: S, entity: Entity): Either[EngineError, S]
  def updateTeam(state: S, team: Team): Either[EngineError, S]
  def updateSurface(state: S, surface: Surface): Either[EngineError, S]
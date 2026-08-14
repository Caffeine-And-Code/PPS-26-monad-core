package monad_core.engine.core.traits

import monad_core.engine.model.*

trait State:

  val UpperLeftCorner  = Vector2D(0.0, 0.0)
  val LowerRightCorner = Vector2D(100.0, 100.0)

  def allEntities: List[Entity]

  def allTeams: List[Team]

  def allSurfaces: List[Surface]

  def getEntity(id: LocatableId): Either[EngineError, Entity]

  def getTeam(id: TeamId): Either[EngineError, Team]

  def getSurface(id: LocatableId): Either[EngineError, Surface]

  def addEntity(entity: Entity): Either[EngineError, State]

  def addTeam(team: Team): Either[EngineError, State]

  def addSurface(surface: Surface): Either[EngineError, State]

  def removeEntity(entity: Entity): Either[EngineError, State]

  def removeTeam(team: Team): Either[EngineError, State]

  def removeSurface(surface: Surface): Either[EngineError, State]

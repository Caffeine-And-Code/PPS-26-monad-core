package monad_core.simulator.infrastructure.ai

import dev.langchain4j.agent.tool.{P, Tool}
import monad_core.simulator.application.engine.*
import monad_core.simulator.application.engine.world.{SaveEntityCommand, SaveSurfaceCommand, SaveTeamCommand, World}
import monad_core.simulator.domain.engine.MonadCoreShape.{SimulationCircle, SimulationRectangle}
import monad_core.simulator.domain.engine.{MonadCoreEntity, MonadCoreSurface, MonadCoreTeam}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.*

case class IncompleteEntitySpeed() extends BaseError("Both speedX and speedY must be provided together")

case class Langchain4jTools()(
  using world: World,
  gameEngineRuntime: GameEngineRuntime
):

  @Tool(Array("Lists all entities in the world."))
  def getAllEntities: String =
    getSafeList(world.getAllEntities)(renderList(_, "entities")(renderEntity))

  @Tool(Array("Gets an entity by its identifier."))
  def getEntity(
                 @P("Entity identifier") id: String
               ): String =
    render(world.getEntity(id))(renderEntity)

  @Tool(Array("Creates a circular entity."))
  def createCircleEntity(
                          @P("Unique entity identifier") id: String,
                          @P("X coordinate") x: Double,
                          @P("Y coordinate") y: Double,
                          @P("Circle radius, greater than zero") radius: Double,
                          @P(value = "Optional team identifier", required = false)
                          teamId: String = null,
                          @P(value = "Optional entity weight, zero or greater", required = false)
                          weight: Integer = null,
                          @P(value = "Optional horizontal speed; provide together with speedY", required = false)
                          speedX: java.lang.Double = null,
                          @P(value = "Optional vertical speed; provide together with speedX", required = false)
                          speedY: java.lang.Double = null,
                          @P(value = "Optional health integer of the entity", required = false)
                          health: java.lang.Integer = null
                        ): String =
    whileEngineStopped {
      validateSpeed(speedX, speedY) match
        case Left(errorMsg) => s"Error: $errorMsg"
        case Right(speedOpt) =>
          save(
            world.createEntity(
              SaveEntityCommand(
                MonadCoreEntity(
                  id = id,
                  position = (x, y),
                  shape = SimulationCircle(radius),
                  speed = speedOpt,
                  health = Option(health).map(_.intValue),
                  weight = Option(weight).map(_.intValue),
                  teamId = Option(teamId)
                )
              )
            ),
            s"Entity '$id' created."
          )
    }

  @Tool(Array("Creates a rectangular entity."))
  def createRectangleEntity(
                             @P("Unique entity identifier") id: String,
                             @P("X coordinate") x: Double,
                             @P("Y coordinate") y: Double,
                             @P("Rectangle height, greater than zero") height: Double,
                             @P("Rectangle length, greater than zero") length: Double,
                             @P(value = "Optional team identifier", required = false)
                             teamId: String = null,
                             @P(value = "Optional entity weight, zero or greater", required = false)
                             weight: Integer = null,
                             @P(value = "Optional horizontal speed; provide together with speedY", required = false)
                             speedX: java.lang.Double = null,
                             @P(value = "Optional vertical speed; provide together with speedX", required = false)
                             speedY: java.lang.Double = null,
                             @P(value = "Optional health integer of the entity", required = false)
                             health: java.lang.Integer = null
                           ): String =
    whileEngineStopped {
      validateSpeed(speedX, speedY) match
        case Left(errorMsg) => s"Error: $errorMsg"
        case Right(speedOpt) =>
          save(
            world.createEntity(
              SaveEntityCommand(
                MonadCoreEntity(
                  id = id,
                  position = (x, y),
                  shape = SimulationRectangle(height = height, width = length),
                  speed = speedOpt,
                  health = Option(health).map(_.intValue),
                  weight = Option(weight).map(_.intValue),
                  teamId = Option(teamId)
                )
              )
            ),
            s"Entity '$id' created."
          )
    }

  @Tool(Array("Replaces an entity with a circular entity having the same identifier."))
  def updateCircleEntity(
                          @P("Identifier of the entity to update") id: String,
                          @P("New X coordinate") x: Double,
                          @P("New Y coordinate") y: Double,
                          @P("New circle radius, greater than zero") radius: Double
                        ): String =
    whileEngineStopped {
      save(
        world.updateEntity(
          SaveEntityCommand(
            MonadCoreEntity(
              id = id,
              position = (x, y),
              shape = SimulationCircle(radius),
            )
          )
        ),
        s"Entity '$id' updated."
      )
    }

  @Tool(Array("Replaces an entity with a rectangular entity having the same identifier."))
  def updateRectangleEntity(
                             @P("Identifier of the entity to update") id: String,
                             @P("New X coordinate") x: Double,
                             @P("New Y coordinate") y: Double,
                             @P("New rectangle height, greater than zero") height: Double,
                             @P("New rectangle length, greater than zero") length: Double
                           ): String =
    whileEngineStopped {
      save(
        world.updateEntity(
          SaveEntityCommand(
            MonadCoreEntity(
              id = id,
              position = (x, y),
              shape = SimulationRectangle(length, height),
            )
          )
        ),
        s"Entity '$id' updated."
      )
    }

  @Tool(Array("Removes an entity by its identifier."))
  def removeEntity(
                    @P("Entity identifier") id: String
                  ): String =
    whileEngineStopped {
      save(
        world.removeEntity(id),
        s"Entity '$id' removed."
      )
    }

  @Tool(Array("Lists all surfaces in the world."))
  def getAllSurfaces: String =
    getSafeList(world.getAllSurfaces)(renderList(_, "surfaces")(renderSurface))

  @Tool(Array("Gets a surface by its identifier."))
  def getSurface(
                  @P("Surface identifier") id: String
                ): String =
    render(world.getSurface(id))(renderSurface)

  @Tool(Array("Creates a circular surface."))
  def createCircleSurface(
                           @P("Unique surface identifier") id: String,
                           @P("X coordinate") x: Double,
                           @P("Y coordinate") y: Double,
                           @P("Circle radius, greater than zero") radius: Double
                         ): String =
    whileEngineStopped {
      save(
        world.createSurface(
          SaveSurfaceCommand(
            MonadCoreSurface(
              id = id,
              position = (x, y),
              shape = SimulationCircle(radius),
            )
          )
        ),
        s"Surface '$id' created."
      )
    }

  @Tool(Array("Creates a rectangular surface."))
  def createRectangleSurface(
                              @P("Unique surface identifier") id: String,
                              @P("X coordinate") x: Double,
                              @P("Y coordinate") y: Double,
                              @P("Rectangle height, greater than zero") height: Double,
                              @P("Rectangle length, greater than zero") length: Double
                            ): String =
    whileEngineStopped {
      save(
        world.createSurface(
          SaveSurfaceCommand(
            MonadCoreSurface(
              id = id,
              position = (x, y),
              shape = SimulationRectangle(length, height),
            )
          )
        ),
        s"Surface '$id' created."
      )
    }

  @Tool(Array("Replaces a surface with a circular surface having the same identifier."))
  def updateCircleSurface(
                           @P("Identifier of the surface to update") id: String,
                           @P("New X coordinate") x: Double,
                           @P("New Y coordinate") y: Double,
                           @P("New circle radius, greater than zero") radius: Double
                         ): String =
    whileEngineStopped {
      save(
        world.updateSurface(
          SaveSurfaceCommand(
            MonadCoreSurface(
              id = id,
              position = (x, y),
              shape = SimulationCircle(radius),
            )
          )
        ),
        s"Surface '$id' updated."
      )
    }

  @Tool(Array("Replaces a surface with a rectangular surface having the same identifier."))
  def updateRectangleSurface(
                              @P("Identifier of the surface to update") id: String,
                              @P("New X coordinate") x: Double,
                              @P("New Y coordinate") y: Double,
                              @P("New rectangle height, greater than zero") height: Double,
                              @P("New rectangle length, greater than zero") length: Double
                            ): String =
    whileEngineStopped {
      save(
        world.updateSurface(
          SaveSurfaceCommand(
            MonadCoreSurface(
              id = id,
              position = (x, y),
              shape = SimulationRectangle(length, height),
            )
          )
        ),
        s"Surface '$id' updated."
      )
    }

  @Tool(Array("Removes a surface by its identifier."))
  def removeSurface(
                     @P("Surface identifier") id: String
                   ): String =
    whileEngineStopped {
      save(
        world.removeSurface(id),
        s"Surface '$id' removed."
      )
    }

  @Tool(Array("Lists all teams in the world."))
  def getAllTeams: String =
    getSafeList(world.getAllTeams)(renderList(_, "teams")(renderTeam))

  @Tool(Array("Gets a team by its identifier."))
  def getTeam(
               @P("Team identifier") id: String
             ): String =
    render(world.getTeam(id))(renderTeam)

  @Tool(Array("Creates a team and optionally assigns enemy teams."))
  def createTeam(
                  @P("Unique team identifier") id: String,
                  @P("Comma-separated enemy team identifiers; use an empty string for none") enemies: String
                ): String =
    whileEngineStopped {
      save(
        world.createTeam(
          SaveTeamCommand(
            MonadCoreTeam(
              id = id,
              enemies = parseIds(enemies)
            )
          )
        ),
        s"Team '$id' created."
      )
    }

  @Tool(Array("Replaces a team's enemy list."))
  def updateTeam(
                  @P("Identifier of the team to update") id: String,
                  @P("New comma-separated enemy team identifiers; use an empty string for none") enemies: String
                ): String =
    whileEngineStopped {
      save(
        world.updateTeam(
          SaveTeamCommand(
            MonadCoreTeam(
              id = id,
              enemies = parseIds(enemies)
            )
          )
        ),
        s"Team '$id' updated."
      )
    }

  @Tool(Array("Removes a team by its identifier."))
  def removeTeam(
                  @P("Team identifier") id: String
                ): String =
    whileEngineStopped {
      save(
        world.removeTeam(id),
        s"Team '$id' removed."
      )
    }

  @Tool(Array("Starts the game engine."))
  def start(): String =
    gameEngineRuntime.start()
    "Game engine started."

  @Tool(Array("Stops the game engine."))
  def stop(): String =
    gameEngineRuntime.stop()
    "Game engine stopped."

  private def whileEngineStopped(operation: => String): String =
    if gameEngineRuntime.isRunning then
      "Error: The world cannot be modified while the game engine is running."
    else operation

  private def validateSpeed(x: java.lang.Double, y: java.lang.Double): Either[String, Option[(Double, Double)]] =
    (Option(x), Option(y)) match
      case (Some(sx), Some(sy)) => Right(Some((sx.doubleValue, sy.doubleValue)))
      case (None, None) => Right(None)
      case _ => Left("Both speedX and speedY must be provided together")

  private def parseIds(csv: String): Set[String] =
    csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet

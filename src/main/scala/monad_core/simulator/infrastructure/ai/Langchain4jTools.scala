package monad_core.simulator.infrastructure.ai

import dev.langchain4j.agent.tool.{P, Tool}
import monad_core.engine.model.*
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.*
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError

case class IncompleteEntitySpeed()
    extends BaseError("Both speedX and speedY must be provided together")

case class Langchain4jTools()(using
    world: World,
    gameEngineRuntime: EngineControl
):

  @Tool(Array("Lists all entities in the world."))
  def getAllEntities: String =
    renderList(world.getAllEntities, "entities")(renderEntity)

  @Tool(Array("Gets an entity by its identifier."))
  def getEntity(
      @P("Entity identifier") id: String
  ): String =
    render(LocatableId(id).adaptError().flatMap(id => world.getEntity(id.value)))(renderEntity)

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
      speedY: java.lang.Double = null
  ): String =
    whileEngineStopped {
      save(
        Entity
          .circle(id, Vector2D(x, y), radius)
          .adaptError()
          .flatMap(withOptionalEntityFields(_, teamId, weight, speedX, speedY))
          .flatMap(entity => world.createEntity(SaveEntityCommand(entity))),
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
      speedY: java.lang.Double = null
  ): String =
    whileEngineStopped {
      save(
        Entity
          .rectangle(id, Vector2D(x, y), height, length)
          .adaptError()
          .flatMap(withOptionalEntityFields(_, teamId, weight, speedX, speedY))
          .flatMap(entity => world.createEntity(SaveEntityCommand(entity))),
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
        Entity
          .circle(id, Vector2D(x, y), radius)
          .adaptError()
          .flatMap(entity => world.updateEntity(SaveEntityCommand(entity))),
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
        Entity
          .rectangle(id, Vector2D(x, y), height, length)
          .adaptError()
          .flatMap(entity => world.updateEntity(SaveEntityCommand(entity))),
        s"Entity '$id' updated."
      )
    }

  @Tool(Array("Removes an entity by its identifier."))
  def removeEntity(
      @P("Entity identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        LocatableId(id).adaptError().flatMap(id => world.removeEntity(id.value)),
        s"Entity '$id' removed."
      )
    }

  @Tool(Array("Lists all surfaces in the world."))
  def getAllSurfaces: String =
    renderList(world.getAllSurfaces, "surfaces")(renderSurface)

  @Tool(Array("Gets a surface by its identifier."))
  def getSurface(
      @P("Surface identifier") id: String
  ): String =
    render(LocatableId(id).adaptError().flatMap(id => world.getSurface(id.value)))(renderSurface)

  @Tool(Array("Creates a circular surface."))
  def createCircleSurface(
      @P("Unique surface identifier") id: String,
      @P("X coordinate") x: Double,
      @P("Y coordinate") y: Double,
      @P("Circle radius, greater than zero") radius: Double
  ): String =
    whileEngineStopped {
      save(
        Surface
          .circle(id, Vector2D(x, y), radius)
          .adaptError()
          .flatMap(surface => world.createSurface(SaveSurfaceCommand(surface))),
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
        Surface
          .rectangle(id, Vector2D(x, y), height, length)
          .adaptError()
          .flatMap(surface => world.createSurface(SaveSurfaceCommand(surface))),
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
        Surface
          .circle(id, Vector2D(x, y), radius)
          .adaptError()
          .flatMap(surface => world.updateSurface(SaveSurfaceCommand(surface))),
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
        Surface
          .rectangle(id, Vector2D(x, y), height, length)
          .adaptError()
          .flatMap(surface => world.updateSurface(SaveSurfaceCommand(surface))),
        s"Surface '$id' updated."
      )
    }

  @Tool(Array("Removes a surface by its identifier."))
  def removeSurface(
      @P("Surface identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        LocatableId(id).adaptError().flatMap(id => world.removeSurface(id.value)),
        s"Surface '$id' removed."
      )
    }

  @Tool(Array("Lists all teams in the world."))
  def getAllTeams: String =
    renderList(world.getAllTeams, "teams")(renderTeam)

  @Tool(Array("Gets a team by its identifier."))
  def getTeam(
      @P("Team identifier") id: String
  ): String =
    render(TeamId(id).adaptError().flatMap(id => world.getTeam(id.value)))(renderTeam)

  @Tool(Array("Creates a team and optionally assigns enemy teams."))
  def createTeam(
      @P("Unique team identifier") id: String,
      @P("Comma-separated enemy team identifiers; use an empty string for none") enemies: String
  ): String =
    whileEngineStopped {
      save(
        Team
          .create(id, parseIds(enemies))
          .adaptError()
          .flatMap(team => world.createTeam(SaveTeamCommand(team))),
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
        Team
          .create(id, parseIds(enemies))
          .adaptError()
          .flatMap(team => world.updateTeam(SaveTeamCommand(team))),
        s"Team '$id' updated."
      )
    }

  @Tool(Array("Removes a team by its identifier."))
  def removeTeam(
      @P("Team identifier") id: String
  ): String =
    whileEngineStopped {
      save(
        TeamId(id).adaptError().flatMap(id => world.removeTeam(id.value)),
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

  private def parseIds(csv: String): Set[String] =
    csv.split(",").iterator.map(_.trim).filter(_.nonEmpty).toSet

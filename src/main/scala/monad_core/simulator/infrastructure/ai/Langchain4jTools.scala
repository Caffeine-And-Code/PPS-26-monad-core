package monad_core.simulator.infrastructure.ai

import dev.langchain4j.agent.tool.{P, Tool}
import monad_core.engine.model.*
import monad_core.simulator.application.engine.EngineControl
import monad_core.simulator.application.engine.errors.ErrorsAdapter.adaptError
import monad_core.simulator.application.engine.world.{
  SaveEntityCommand,
  SaveSurfaceCommand,
  SaveTeamCommand,
  World
}
import monad_core.simulator.errors.BaseError
import monad_core.simulator.infrastructure.ai.Langchain4jToolOptions.applyTo
import monad_core.simulator.infrastructure.ai.Langchain4jToolResponse.*

case class Langchain4jTools()(using
    world: World,
    gameEngineRuntime: EngineControl
):

  private val defaultRotation = 0.0

  private val rotationOrDefault: java.lang.Double => Double =
    rotation => Option(rotation).fold(defaultRotation)(_.doubleValue())

  @Tool(Array("Lists all entities in the world."))
  def getAllEntities: String =
    renderList(world.getAllEntities, "entities")(renderEntity)

  @Tool(Array("Gets an entity by its identifier."))
  def getEntity(
      @P("Entity identifier") id: String
  ): String =
    render(LocatableId(id).adaptError().flatMap(id => world.getEntity(id.value)))(renderEntity)

  /**
   * Completes an entity initialization by adapting construction errors and applying its optional fields.
   *
   * @see [[monad_core.simulator.application.engine.errors.EngineErrorAdapted]]
   * @param previousResult the entity construction result
   * @param optionalFields the optional properties to apply
   * @return `Left(BaseError)` on failure, `Right(Entity)` with the initialized entity otherwise
   */
  private def initializeEntity(
      previousResult: Either[EngineError, Entity],
      optionalFields: EntityOptionalFields
  ): Either[BaseError, Entity] =
    previousResult
      .adaptError()
      .flatMap(applyTo(_, optionalFields))

  /**
   * Initializes a rectangular entity, using the default rotation when none is provided.
   *
   * @param id the unique identifier of the entity
   * @param x the horizontal position
   * @param y the vertical position
   * @param height the rectangle height
   * @param length the rectangle length
   * @param optionalFields the optional properties to apply
   * @param rotation the initial rotation, or `null` to use the default
   * @return `Left(BaseError)` on failure, `Right(Entity)` with the initialized entity otherwise
   */
  private def initializeRectangleEntity(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      optionalFields: EntityOptionalFields,
      rotation: java.lang.Double = null
  ): Either[BaseError, Entity] =
    val initialRotation = rotationOrDefault(rotation)

    initializeEntity(
      Entity
        .rectangle(id, Vector2D(x, y), height, length, initialRotation),
      optionalFields
    )

  /**
   * Initializes a circular entity, using the default rotation when none is provided.
   *
   * @param id the unique identifier of the entity
   * @param x the horizontal position
   * @param y the vertical position
   * @param radius the circle radius
   * @param optionalFields the optional properties to apply
   * @param rotation the initial rotation, or `null` to use the default
   * @return `Left(BaseError)` on failure, `Right(Entity)` with the initialized entity otherwise
   */
  private def initializeCircleEntity(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      optionalFields: EntityOptionalFields,
      rotation: java.lang.Double = null
  ): Either[BaseError, Entity] =
    val initialRotation = rotationOrDefault(rotation)

    initializeEntity(
      Entity
        .circle(id, Vector2D(x, y), radius, initialRotation),
      optionalFields
    )

  /**
   * Completes a surface initialization by adapting construction errors and applying its optional fields.
   *
   * @see [[monad_core.simulator.application.engine.errors.EngineErrorAdapted]]
   * @param previousResult the surface construction result
   * @param optionalFields the optional properties to apply
   * @return `Left(BaseError)` on failure, `Right(Surface)` with the initialized surface otherwise
   */
  private def initializeSurface(
      previousResult: Either[EngineError, Surface],
      optionalFields: SurfaceOptionalFields
  ): Either[BaseError, Surface] =
    previousResult
      .adaptError()
      .flatMap(applyTo(_, optionalFields))

  /**
   * Initializes a rectangular surface, using the default rotation when none is provided.
   *
   * @param id the unique identifier of the surface
   * @param x the horizontal position
   * @param y the vertical position
   * @param height the rectangle height
   * @param length the rectangle length
   * @param optionalFields the optional properties to apply
   * @param rotation the initial rotation, or `null` to use the default
   * @return `Left(BaseError)` on failure, `Right(Surface)` with the initialized surface otherwise
   */
  private def initializeRectangleSurface(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      optionalFields: SurfaceOptionalFields,
      rotation: java.lang.Double = null
  ): Either[BaseError, Surface] =
    initializeSurface(
      Surface.rectangle(id, Vector2D(x, y), height, length, rotationOrDefault(rotation)),
      optionalFields
    )

  /**
   * Initializes a circular surface, using the default rotation when none is provided.
   *
   * @param id the unique identifier of the surface
   * @param x the horizontal position
   * @param y the vertical position
   * @param radius the circle radius
   * @param optionalFields the optional properties to apply
   * @param rotation the initial rotation, or `null` to use the default
   * @return `Left(BaseError)` on failure, `Right(Surface)` with the initialized surface otherwise
   */
  private def initializeCircleSurface(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      optionalFields: SurfaceOptionalFields,
      rotation: java.lang.Double = null
  ): Either[BaseError, Surface] =
    initializeSurface(
      Surface.circle(id, Vector2D(x, y), radius, rotationOrDefault(rotation)),
      optionalFields
    )

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
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional initial health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeCircleEntity(
          id,
          x,
          y,
          radius,
          EntityOptionalFields(
            teamId = teamId,
            weight = weight,
            speedX = speedX,
            speedY = speedY,
            angularSpeed = angularSpeed,
            health = health,
            damage = damage
          ),
          rotation
        )
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
      speedY: java.lang.Double = null,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional initial health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeRectangleEntity(
          id,
          x,
          y,
          height,
          length,
          EntityOptionalFields(
            teamId = teamId,
            weight = weight,
            speedX = speedX,
            speedY = speedY,
            angularSpeed = angularSpeed,
            health = health,
            damage = damage
          ),
          rotation
        )
          .flatMap(entity => world.createEntity(SaveEntityCommand(entity))),
        s"Entity '$id' created."
      )
    }

  @Tool(Array("Replaces an entity with a circular entity having the same identifier."))
  def updateCircleEntity(
      @P("Identifier of the entity to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New circle radius, greater than zero") radius: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional team identifier", required = false)
      teamId: String = null,
      @P(value = "Optional entity weight, zero or greater", required = false)
      weight: Integer = null,
      @P(value = "Optional horizontal speed; provide together with speedY", required = false)
      speedX: java.lang.Double = null,
      @P(value = "Optional vertical speed; provide together with speedX", required = false)
      speedY: java.lang.Double = null,
      @P(value = "Optional new health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional new damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeCircleEntity(
          id,
          x,
          y,
          radius,
          EntityOptionalFields(
            teamId = teamId,
            weight = weight,
            speedX = speedX,
            speedY = speedY,
            angularSpeed = angularSpeed,
            health = health,
            damage = damage
          ),
          rotation
        )
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
      @P("New rectangle length, greater than zero") length: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new angular speed in degrees per second", required = false)
      angularSpeed: java.lang.Double = null,
      @P(value = "Optional team identifier", required = false)
      teamId: String = null,
      @P(value = "Optional entity weight, zero or greater", required = false)
      weight: Integer = null,
      @P(value = "Optional horizontal speed; provide together with speedY", required = false)
      speedX: java.lang.Double = null,
      @P(value = "Optional vertical speed; provide together with speedX", required = false)
      speedY: java.lang.Double = null,
      @P(value = "Optional new health of the entity", required = false)
      health: java.lang.Integer = null,
      @P(value = "Optional new damage dealt by the entity", required = false)
      damage: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        result = initializeRectangleEntity(
          id,
          x,
          y,
          height,
          length,
          EntityOptionalFields(
            teamId = teamId,
            weight = weight,
            speedX = speedX,
            speedY = speedY,
            angularSpeed = angularSpeed,
            health = health,
            damage = damage
          ),
          rotation
        )
          .flatMap(entity => world.updateEntity(SaveEntityCommand(entity))),
        successMessage = s"Entity '$id' updated."
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
      @P("Circle radius, greater than zero") radius: Double,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeCircleSurface(
          id,
          x,
          y,
          radius,
          SurfaceOptionalFields(
            frictionIndex,
            appliedForceX,
            appliedForceY,
            damageOverTime
          ),
          rotation
        )
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
      @P("Rectangle length, greater than zero") length: Double,
      @P(value = "Optional initial rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeRectangleSurface(
          id,
          x,
          y,
          height,
          length,
          SurfaceOptionalFields(
            frictionIndex,
            appliedForceX,
            appliedForceY,
            damageOverTime
          ),
          rotation
        )
          .flatMap(surface => world.createSurface(SaveSurfaceCommand(surface))),
        s"Surface '$id' created."
      )
    }

  @Tool(Array("Replaces a surface with a circular surface having the same identifier."))
  def updateCircleSurface(
      @P("Identifier of the surface to update") id: String,
      @P("New X coordinate") x: Double,
      @P("New Y coordinate") y: Double,
      @P("New circle radius, greater than zero") radius: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional new horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional new vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional new damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeCircleSurface(
          id,
          x,
          y,
          radius,
          SurfaceOptionalFields(
            frictionIndex,
            appliedForceX,
            appliedForceY,
            damageOverTime
          ),
          rotation
        )
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
      @P("New rectangle length, greater than zero") length: Double,
      @P(value = "Optional new rotation in degrees, between 0 and 360", required = false)
      rotation: java.lang.Double = null,
      @P(value = "Optional new friction index", required = false)
      frictionIndex: java.lang.Double = null,
      @P(
        value = "Optional new horizontal applied force; provide together with appliedForceY",
        required = false
      )
      appliedForceX: java.lang.Double = null,
      @P(
        value = "Optional new vertical applied force; provide together with appliedForceX",
        required = false
      )
      appliedForceY: java.lang.Double = null,
      @P(value = "Optional new damage over time dealt by the surface", required = false)
      damageOverTime: java.lang.Integer = null
  ): String =
    whileEngineStopped {
      save(
        initializeRectangleSurface(
          id,
          x,
          y,
          height,
          length,
          SurfaceOptionalFields(
            frictionIndex,
            appliedForceX,
            appliedForceY,
            damageOverTime
          ),
          rotation
        )
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

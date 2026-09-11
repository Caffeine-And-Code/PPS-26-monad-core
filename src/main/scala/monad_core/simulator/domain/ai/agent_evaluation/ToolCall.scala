package monad_core.simulator.domain.ai.agent_evaluation

/** Typed representation of all the possible tool invocations made by the AI agent. */
enum ToolCall:
  /** Reads every entity. */
  case GetAllEntities

  /** @param id entity identifier */
  case GetEntity(id: String)

  /**
   * @param id entity identifier
   * @param x horizontal position
   * @param y vertical position
   * @param radius radius
   * @param teamId optional team
   * @param weight optional weight
   * @param speedX optional horizontal speed
   * @param speedY optional vertical speed
   * @param rotation rotation
   * @param angularSpeed optional angular speed
   */
  case CreateCircleEntity(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      teamId: Option[String] = None,
      weight: Option[Int] = None,
      speedX: Option[Double] = None,
      speedY: Option[Double] = None,
      rotation: Double = 0.0,
      angularSpeed: Option[Double] = None
  )

  /**
   * @param id entity identifier
   * @param x horizontal position
   * @param y vertical position
   * @param height height
   * @param length length
   * @param teamId optional team
   * @param weight optional weight
   * @param speedX optional horizontal speed
   * @param speedY optional vertical speed
   * @param rotation rotation
   * @param angularSpeed optional angular speed
   */
  case CreateRectangleEntity(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      teamId: Option[String] = None,
      weight: Option[Int] = None,
      speedX: Option[Double] = None,
      speedY: Option[Double] = None,
      rotation: Double = 0.0,
      angularSpeed: Option[Double] = None
  )

  /**
   * @param id entity identifier
   * @param x horizontal position
   * @param y vertical position
   * @param radius radius
   * @param rotation rotation
   */
  case UpdateCircleEntity(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      rotation: Double = 0.0
  )

  /**
   * @param id entity identifier
   * @param x horizontal position
   * @param y vertical position
   * @param height height
   * @param length length
   * @param rotation rotation
   */
  case UpdateRectangleEntity(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      rotation: Double = 0.0
  )

  /** @param id entity identifier */
  case RemoveEntity(id: String)

  /** Reads every surface. */
  case GetAllSurfaces

  /** @param id surface identifier */
  case GetSurface(id: String)

  /**
   * @param id surface identifier
   * @param x horizontal position
   * @param y vertical position
   * @param radius radius
   * @param rotation rotation
   */
  case CreateCircleSurface(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      rotation: Double = 0.0
  )

  /**
   * @param id surface identifier
   * @param x horizontal position
   * @param y vertical position
   * @param height height
   * @param length length
   * @param rotation rotation
   */
  case CreateRectangleSurface(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      rotation: Double = 0.0
  )

  /**
   * @param id surface identifier
   * @param x horizontal position
   * @param y vertical position
   * @param radius radius
   * @param rotation rotation
   */
  case UpdateCircleSurface(
      id: String,
      x: Double,
      y: Double,
      radius: Double,
      rotation: Double = 0.0
  )

  /**
   * @param id surface identifier
   * @param x horizontal position
   * @param y vertical position
   * @param height height
   * @param length length
   * @param rotation rotation
   */
  case UpdateRectangleSurface(
      id: String,
      x: Double,
      y: Double,
      height: Double,
      length: Double,
      rotation: Double = 0.0
  )

  /** @param id surface identifier */
  case RemoveSurface(id: String)

  /** Reads every team. */
  case GetAllTeams

  /** @param id team identifier */
  case GetTeam(id: String)

  /**
   * @param id team identifier
   * @param enemies comma-separated enemy identifiers
   */
  case CreateTeam(id: String, enemies: String)

  /**
   * @param id team identifier
   * @param enemies comma-separated enemy identifiers
   */
  case UpdateTeam(id: String, enemies: String)

  /** @param id team identifier */
  case RemoveTeam(id: String)

  /** Starts the engine. */
  case Start

  /** Stops the engine. */
  case Stop

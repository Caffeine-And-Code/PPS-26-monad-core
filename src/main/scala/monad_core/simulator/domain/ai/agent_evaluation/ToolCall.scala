package monad_core.simulator.domain.ai.agent_evaluation

enum ToolCall:
  case GetAllEntities
  case GetEntity(id: String)
  case CreateCircleEntity(
    id: String,
    x: Double,
    y: Double,
    radius: Double,
    teamId: Option[String] = None,
    weight: Option[Int] = None,
    speedX: Option[Double] = None,
    speedY: Option[Double] = None
  )
  case CreateRectangleEntity(
    id: String,
    x: Double,
    y: Double,
    height: Double,
    length: Double,
    teamId: Option[String] = None,
    weight: Option[Int] = None,
    speedX: Option[Double] = None,
    speedY: Option[Double] = None
  )
  case UpdateCircleEntity(id: String, x: Double, y: Double, radius: Double)
  case UpdateRectangleEntity(id: String, x: Double, y: Double, height: Double, length: Double)
  case RemoveEntity(id: String)

  case GetAllSurfaces
  case GetSurface(id: String)
  case CreateCircleSurface(id: String, x: Double, y: Double, radius: Double)
  case CreateRectangleSurface(id: String, x: Double, y: Double, height: Double, length: Double)
  case UpdateCircleSurface(id: String, x: Double, y: Double, radius: Double)
  case UpdateRectangleSurface(id: String, x: Double, y: Double, height: Double, length: Double)
  case RemoveSurface(id: String)

  case GetAllTeams
  case GetTeam(id: String)
  case CreateTeam(id: String, enemies: String)
  case UpdateTeam(id: String, enemies: String)
  case RemoveTeam(id: String)

  case Start
  case Stop

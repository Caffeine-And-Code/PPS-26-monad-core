package monad_core.simulator.infrastructure.ai.agent_evaluator

import dev.langchain4j.agent.tool.ToolExecutionRequest
import dev.langchain4j.internal.Json
import monad_core.simulator.domain.ai.agent_evaluation.ToolCall
import monad_core.simulator.errors.BaseError

import scala.jdk.CollectionConverters.*
import scala.util.Try

case class InvalidToolCall(toolName: String, reason: String)
    extends BaseError(s"Cannot map tool '$toolName': $reason")

case class Langchain4jToolCallMapper():

  def from(request: ToolExecutionRequest): Either[BaseError, ToolCall] =
    request.name() match
      case "getAllEntities" => Right(ToolCall.GetAllEntities)
      case "getEntity"      => withArguments(request)(arguments => getEntity(arguments))
      case "createCircleEntity" =>
        withArguments(request)(arguments => createCircleEntity(arguments))
      case "createRectangleEntity" =>
        withArguments(request)(arguments => createRectangleEntity(arguments))
      case "updateCircleEntity" =>
        withArguments(request)(arguments => updateCircleEntity(arguments))
      case "updateRectangleEntity" =>
        withArguments(request)(arguments => updateRectangleEntity(arguments))
      case "removeEntity"   => withArguments(request)(arguments => removeEntity(arguments))
      case "getAllSurfaces" => Right(ToolCall.GetAllSurfaces)
      case "getSurface"     => withArguments(request)(arguments => getSurface(arguments))
      case "createCircleSurface" =>
        withArguments(request)(arguments => createCircleSurface(arguments))
      case "createRectangleSurface" =>
        withArguments(request)(arguments => createRectangleSurface(arguments))
      case "updateCircleSurface" =>
        withArguments(request)(arguments => updateCircleSurface(arguments))
      case "updateRectangleSurface" =>
        withArguments(request)(arguments => updateRectangleSurface(arguments))
      case "removeSurface" => withArguments(request)(arguments => removeSurface(arguments))
      case "getAllTeams"   => Right(ToolCall.GetAllTeams)
      case "getTeam"       => withArguments(request)(arguments => getTeam(arguments))
      case "createTeam"    => withArguments(request)(arguments => createTeam(arguments))
      case "updateTeam"    => withArguments(request)(arguments => updateTeam(arguments))
      case "removeTeam"    => withArguments(request)(arguments => removeTeam(arguments))
      case "start"         => Right(ToolCall.Start)
      case "stop"          => Right(ToolCall.Stop)
      case toolName        => Left(InvalidToolCall(toolName, "unknown tool"))

  private type Arguments = Map[String, Any]

  private def withArguments(
      request: ToolExecutionRequest
  )(
      mapper: Arguments => Either[BaseError, ToolCall]
  ): Either[BaseError, ToolCall] =
    parseArguments(request)
      .flatMap(mapper)
      .left
      .map:
        case InvalidToolCall(_, reason) => InvalidToolCall(request.name(), reason)
        case error                      => error

  private def parseArguments(request: ToolExecutionRequest): Either[BaseError, Arguments] =
    Try {
      Json
        .fromJson(
          Option(request.arguments()).getOrElse("{}"),
          classOf[java.util.Map[?, ?]]
        )
        .asScala
        .iterator
        .foldLeft(Right(Map.empty): Either[BaseError, Arguments]):
          case (arguments, (key: String, value)) =>
            arguments.map(_ + (key -> value))
          case (_, (key, _)) =>
            Left(InvalidToolCall(request.name(), s"invalid argument name '$key'"))
    }.toEither.left
      .map(error => InvalidToolCall(request.name(), error.getMessage))
      .flatMap(result => result)

  private def getEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.GetEntity.apply)

  private def createCircleEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      radius <- double(arguments, "radius")
      teamId <- optionalString(arguments, "teamId")
      weight <- optionalInt(arguments, "weight")
      speedX <- optionalDouble(arguments, "speedX")
      speedY <- optionalDouble(arguments, "speedY")
    yield ToolCall.CreateCircleEntity(id, x, y, radius, teamId, weight, speedX, speedY)

  private def createRectangleEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      height <- double(arguments, "height")
      length <- double(arguments, "length")
      teamId <- optionalString(arguments, "teamId")
      weight <- optionalInt(arguments, "weight")
      speedX <- optionalDouble(arguments, "speedX")
      speedY <- optionalDouble(arguments, "speedY")
    yield ToolCall.CreateRectangleEntity(id, x, y, height, length, teamId, weight, speedX, speedY)

  private def updateCircleEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      radius <- double(arguments, "radius")
    yield ToolCall.UpdateCircleEntity(id, x, y, radius)

  private def updateRectangleEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      height <- double(arguments, "height")
      length <- double(arguments, "length")
    yield ToolCall.UpdateRectangleEntity(id, x, y, height, length)

  private def removeEntity(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.RemoveEntity.apply)

  private def getSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.GetSurface.apply)

  private def createCircleSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      radius <- double(arguments, "radius")
    yield ToolCall.CreateCircleSurface(id, x, y, radius)

  private def createRectangleSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      height <- double(arguments, "height")
      length <- double(arguments, "length")
    yield ToolCall.CreateRectangleSurface(id, x, y, height, length)

  private def updateCircleSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      radius <- double(arguments, "radius")
    yield ToolCall.UpdateCircleSurface(id, x, y, radius)

  private def updateRectangleSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id     <- string(arguments, "id")
      x      <- double(arguments, "x")
      y      <- double(arguments, "y")
      height <- double(arguments, "height")
      length <- double(arguments, "length")
    yield ToolCall.UpdateRectangleSurface(id, x, y, height, length)

  private def removeSurface(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.RemoveSurface.apply)

  private def getTeam(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.GetTeam.apply)

  private def createTeam(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id      <- string(arguments, "id")
      enemies <- string(arguments, "enemies")
    yield ToolCall.CreateTeam(id, enemies)

  private def updateTeam(arguments: Arguments): Either[BaseError, ToolCall] =
    for
      id      <- string(arguments, "id")
      enemies <- string(arguments, "enemies")
    yield ToolCall.UpdateTeam(id, enemies)

  private def removeTeam(arguments: Arguments): Either[BaseError, ToolCall] =
    string(arguments, "id").map(ToolCall.RemoveTeam.apply)

  private def string(arguments: Arguments, name: String): Either[BaseError, String] =
    arguments.get(name) match
      case Some(value: String) => Right(value)
      case _                   => Left(InvalidToolCall("arguments", s"missing or invalid '$name'"))

  private def double(arguments: Arguments, name: String): Either[BaseError, Double] =
    arguments.get(name) match
      case Some(value: Number) => Right(value.doubleValue())
      case _                   => Left(InvalidToolCall("arguments", s"missing or invalid '$name'"))

  private def optionalString(
      arguments: Arguments,
      name: String
  ): Either[BaseError, Option[String]] =
    arguments.get(name) match
      case None | Some(null)   => Right(None)
      case Some(value: String) => Right(Some(value))
      case _                   => Left(InvalidToolCall("arguments", s"invalid '$name'"))

  private def optionalInt(arguments: Arguments, name: String): Either[BaseError, Option[Int]] =
    arguments.get(name) match
      case None | Some(null)   => Right(None)
      case Some(value: Number) => Right(Some(value.intValue()))
      case _                   => Left(InvalidToolCall("arguments", s"invalid '$name'"))

  private def optionalDouble(
      arguments: Arguments,
      name: String
  ): Either[BaseError, Option[Double]] =
    arguments.get(name) match
      case None | Some(null)   => Right(None)
      case Some(value: Number) => Right(Some(value.doubleValue()))
      case _                   => Left(InvalidToolCall("arguments", s"invalid '$name'"))

package monad_core.simulator.presentation.routes

import monad_core.simulator.errors.BaseError

case class RouteResponse(
    success: Boolean,
    message: String
)

enum RouteType:
  case All()
  case Route(string: String)

case class ArgumentRoutingRoute(
    route: RouteType,
    routeRender: () => RouteResponse
)

case class Router(
    routes: Seq[ArgumentRoutingRoute] = Seq()
)

case class RouteNotFoundError() extends BaseError("No route find")

object Router:

  extension (builder: Router)

    def on(route: RouteType, routeRender: () => RouteResponse): Router =
      val routes = builder.routes :+ ArgumentRoutingRoute(route, routeRender)
      builder.copy(routes)

    def evaluate(args: Array[String]): Either[RouteNotFoundError, RouteResponse] =
      builder.routes.find(_.route match
        case RouteType.Route(routeName) => args.contains(routeName)
        case RouteType.All()            => true
      ) match
        case Some(route) => Right(route.routeRender())
        case None        => Left(RouteNotFoundError())

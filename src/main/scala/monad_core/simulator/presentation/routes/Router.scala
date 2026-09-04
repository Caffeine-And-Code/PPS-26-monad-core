package monad_core.simulator.presentation.routes

import monad_core.simulator.errors.BaseError

/**
 * the result and the message returned by a sub-application
 *
 * @param success whether route execution succeeded
 * @param message result description
 */
case class RouteResponse(
    success: Boolean,
    message: String
)

/** Matching rule used by an argument route. */
enum RouteType:

  /** Matches every argument list. */
  case All()

  /**
   * Match a specific route
   *
   * @param string command-line token that activates the route */
  case Route(string: String)

/**
 * the name of the route and his action
 *
 * @param route matching rule
 * @param routeRender action executed after a match
 */
case class ArgumentRoutingRoute(
    route: RouteType,
    routeRender: () => RouteResponse
)

/**
 * Router builder instance
 *
 * @param routes ordered routes evaluated by the router
 */
case class Router(
    routes: Seq[ArgumentRoutingRoute] = Seq()
)

/** Error returned when no configured route matches the arguments. */
case class RouteNotFoundError() extends BaseError("No route find")

/** Immutable [[Router]] with compone pattern methods. */
object Router:

  extension (builder: Router)

    /**
     * Appends a route after the routes already registered.
     *
     * @param route matching rule
     * @param routeRender action executed on a match
     * @return router containing the new route
     */
    def on(route: RouteType, routeRender: () => RouteResponse): Router =
      val routes = builder.routes :+ ArgumentRoutingRoute(route, routeRender)
      builder.copy(routes)

    /**
     * Executes the first route that matches the supplied arguments.
     *
     * @param args command-line arguments
     * @return route response, or [[RouteNotFoundError]] when no route matches
     */
    def evaluate(args: Array[String]): Either[RouteNotFoundError, RouteResponse] =
      builder.routes.find(_.route match
        case RouteType.Route(routeName) => args.contains(routeName)
        case RouteType.All()            => true
      ) match
        case Some(route) => Right(route.routeRender())
        case None        => Left(RouteNotFoundError())

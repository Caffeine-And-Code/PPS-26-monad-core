package monad_core.simulator.presentation.routes

import monad_core.simulator.presentation.routes.RouteType.{All, Route}
import monad_core.simulator.presentation.routes.{RouteNotFoundError, RouteResponse, Router}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RouterTest extends AnyFunSuite with Matchers:

  val successRouteResponse = RouteResponse(success = true, message = "message")

  test("route list is initially empty"):
    val router = Router()

    router.routes.isEmpty shouldBe true

  test("can add routes to route list"):
    val router = Router()
    val routeR1 = Route("r1")
    val allRoute = All()

    val result = router
      .on(routeR1, () => successRouteResponse)
      .on(allRoute, () => successRouteResponse)

    result.routes.length shouldBe 2
    result.routes.map(_.route) shouldBe Seq(routeR1, allRoute)
    result.routes.map(_.routeRender()) shouldBe Seq(successRouteResponse, successRouteResponse)

  test("can evaluate witch route to use based on args"):
    val router = Router()
    val routeToChoose = "r1"
    val anotherRoute = "r2"
    val args: Array[String] = Array(routeToChoose)
    val routeResponseToChoose = RouteResponse(true, "right")

    val result: Either[RouteNotFoundError, RouteResponse] = router
      .on(Route(anotherRoute), () => successRouteResponse)
      .on(Route(routeToChoose), () => routeResponseToChoose)
      .evaluate(args)

    result shouldBe Right(routeResponseToChoose)

  test("the Route All is a flag for indicating all routes"):
    val router = Router()
    val routeToChoose = All()
    val anotherRoute = "r2"
    val args: Array[String] = Array()
    val routeResponseToChoose = RouteResponse(true, "right")

    val result: Either[RouteNotFoundError, RouteResponse] = router
      .on(Route(anotherRoute), () => successRouteResponse)
      .on(routeToChoose, () => routeResponseToChoose)
      .evaluate(args)

    result shouldBe Right(routeResponseToChoose)

  test("when no route is found it returns an Error"):
    val router = Router()
    val anotherRoute = "r2"
    val args: Array[String] = Array()

    val result: Either[RouteNotFoundError, RouteResponse] = router
      .on(Route(anotherRoute), () => successRouteResponse)
      .evaluate(args)

    result shouldBe Left(RouteNotFoundError())
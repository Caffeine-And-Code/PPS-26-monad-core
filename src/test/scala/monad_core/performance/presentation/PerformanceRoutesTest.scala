package monad_core.performance.presentation

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PerformanceRoutesTest extends AnyFunSuite with Matchers:
  test("performance routes should be defined with specific values"):
    val routes = PerformanceRoutes

    val load        = routes.Load
    val stress      = routes.Stress
    val spike       = routes.Spike
    val scalability = routes.Scalability

    load shouldBe "performance-load-test"
    stress shouldBe "performance-stress-test"
    spike shouldBe "performance-spike-test"
    scalability shouldBe "performance-scalability-test"

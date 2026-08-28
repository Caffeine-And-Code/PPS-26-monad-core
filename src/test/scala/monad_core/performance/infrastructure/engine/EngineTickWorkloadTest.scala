package monad_core.performance.infrastructure.engine

import monad_core.performance.application.SampleCollector
import monad_core.performance.domain.{EntityCount, InvalidEntityCount, PerformanceError}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EngineTickWorkloadTest extends AnyFunSuite with Matchers:

  private def prepareWorkload(
      entityCount: Int
  ): Either[PerformanceError, SampleCollector.Operation] =
    EntityCount.from(entityCount).flatMap(EngineTickWorkload.prepare)

  test("an invalid entity count should prevent workload preparation"):
    val invalidEntityCount = 0

    val result = prepareWorkload(invalidEntityCount)

    result shouldBe Left(InvalidEntityCount(invalidEntityCount))

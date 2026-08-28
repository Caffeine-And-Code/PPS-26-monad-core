package monad_core.performance.application

import monad_core.performance.domain.{EntityCount, PerformanceError}

/**
 * Defines the workload measured by a performance experiment.
 *
 * Preparation is deliberately separated from execution so that scene construction and other
 * setup costs are excluded from the collected latency samples. A prepared operation may retain
 * the state needed by subsequent invocations.
 */
trait PerformanceWorkload:

  /**
   * Prepares an operation for a specific workload size.
   *
   * @param entityCount
   *   number of entities that the operation must process
   * @return
   *   a callable operation on success, or a [[monad_core.performance.domain.PerformanceError]] if
   *   the workload cannot be initialized
   */
  def prepare(entityCount: EntityCount): Either[PerformanceError, SampleCollector.Operation]

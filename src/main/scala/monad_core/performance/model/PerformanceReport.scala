package monad_core.performance.model

final case class LatencyDistribution(
                                      p50Nanos: Long,
                                      p95Nanos: Long,
                                      p99Nanos: Long
                                    )

final case class PerformancePoint(
                                   entityCount: EntityCount,
                                   latency: LatencyDistribution,
                                   frameBudgetCompletionRate: Double
                                 )

final case class PerformanceReport(
                                    kind: PerformanceKind,
                                    points: Vector[PerformancePoint],
                                    breakpoint: Option[EntityCount]
                                  )
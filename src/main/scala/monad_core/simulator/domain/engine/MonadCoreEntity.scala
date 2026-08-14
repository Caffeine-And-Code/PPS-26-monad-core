package monad_core.simulator.domain.engine

case class MonadCoreEntity(
    id: String,
    position: (Double, Double),
    shape: MonadCoreShape,
    speed: Option[(Double, Double)] = Option.empty,
    weight: Option[Int] = Option.empty,
    health: Option[Int] = Option.empty,
    teamId: Option[String] = Option.empty
)

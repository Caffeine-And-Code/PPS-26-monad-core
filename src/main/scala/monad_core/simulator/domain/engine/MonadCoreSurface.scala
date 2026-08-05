package monad_core.simulator.domain.engine

case class MonadCoreSurface(
                             id: String,
                             position: (Double, Double),
                             shape: MonadCoreShape,
                             frictionIndex: Option[Double] = Option.empty,
                             appliedForce: Option[(Double, Double)] = Option.empty
                           )

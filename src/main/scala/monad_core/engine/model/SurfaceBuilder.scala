package monad_core.engine.model

object SurfaceBuilder:

  extension (surface: Either[EngineError, Surface])

    def withFrictionIndex(frictionIndex: Option[Double]): Either[EngineError, Surface] =
      surface.flatMap(_.withFrictionIndex(frictionIndex))

    def withAppliedForce(appliedForce: Option[Vector2D]): Either[EngineError, Surface] =
      surface.flatMap(_.withAppliedForce(appliedForce))

    def withDamageOverTime(damageOverTime: Option[Int]): Either[EngineError, Surface] =
      surface.flatMap(_.withDamageOverTime(damageOverTime))

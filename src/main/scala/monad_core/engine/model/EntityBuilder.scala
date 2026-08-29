package monad_core.engine.model

object EntityBuilder:

  extension (entity: Either[EngineError, Entity])

    def withSpeed(speed: Option[Vector2D]): Either[EngineError, Entity] =
      entity.map(_.withSpeed(speed))

    def withAngularSpeed(angularSpeed: Option[Double]): Either[EngineError, Entity] =
      entity.map(_.withAngularSpeed(angularSpeed))

    def withWeight(weight: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withWeight(weight))

    def withHealth(health: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withHealth(health))

    def withDamage(damage: Option[Int]): Either[EngineError, Entity] =
      entity.flatMap(_.withDamage(damage))

    def withTeamId(teamId: Option[String]): Either[EngineError, Entity] =
      entity.flatMap(_.withTeamId(teamId))

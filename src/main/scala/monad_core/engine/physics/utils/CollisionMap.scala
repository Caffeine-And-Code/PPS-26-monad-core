package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Entity

type CollisionMap = Map[Entity, List[(Entity, Collision)]]

package monad_core.engine.physics.utils

import monad_core.engine.geometry.Collision
import monad_core.engine.model.Entity

/** Collisions grouped by the entity whose response must be resolved. */
type CollisionMap = Map[Entity, List[(Entity, Collision)]]

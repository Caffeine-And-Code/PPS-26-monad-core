package monad_core.simulator.domain.engine

case class MonadCoreScene(
                           entities: List[MonadCoreEntity] = List.empty,
                           teams: List[MonadCoreTeam] = List.empty,
                           surfaces: List[MonadCoreSurface] = List.empty
                         )

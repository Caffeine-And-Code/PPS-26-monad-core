package helpers.arrangers

import monad_core.simulator.domain.engine.MonadCoreTeam

object MonadCoreTeamArranger:
  val RedTeamId: String = "RedTeam"
  val BlueTeamId: String = "BlueTeam"
  val GreenTeamId: String = "GreenTeam"
  
  def arrangeTeams : Seq[MonadCoreTeam] =
    Seq(
      arrangeRedTeamWithoutEnemies,
      MonadCoreTeam(BlueTeamId, Set.empty),
      MonadCoreTeam(GreenTeamId, Set.empty)
    )

  private def arrangeTeamWithoutEnemies(id: String): MonadCoreTeam =
    MonadCoreTeam(id, Set.empty)

  def arrangeRedTeamWithoutEnemies : MonadCoreTeam =
    arrangeTeamWithoutEnemies(RedTeamId)
  
  def arrangeBlueTeamWithoutEnemies : MonadCoreTeam =
    arrangeTeamWithoutEnemies(BlueTeamId)

  def arrangeGreenTeamWithoutEnemies : MonadCoreTeam =
    arrangeTeamWithoutEnemies(GreenTeamId)

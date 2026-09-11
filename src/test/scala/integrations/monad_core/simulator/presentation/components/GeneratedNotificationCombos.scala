package integrations.monad_core.simulator.presentation.components

import helpers.prolog.TuProlog
import monad_core.simulator.presentation.components.{Error, Info, NotificationType, Success}
import org.scalatest.prop.TableFor2
import org.scalatest.prop.Tables.Table

object GeneratedNotificationCombos:

  private val NotificationTheory = "/prolog/notification_tuple_combos.pl"
  private val NotificationGoal   = "notification_pair(First, Second)."
  private val ExpectedCaseCount  = 9

  private val generatedCases: Seq[(NotificationType, NotificationType)] =
    TuProlog
      .solve(NotificationTheory, NotificationGoal)
      .map: solution =>
        notificationType(solution.getTerm("First").toString) ->
          notificationType(solution.getTerm("Second").toString)
      .toList

  require(
    generatedCases.size == ExpectedCaseCount,
    s"Expected $ExpectedCaseCount notification combinations, found ${generatedCases.size}"
  )

  val cases: TableFor2[NotificationType, NotificationType] = Table(
    ("firstMessageType", "secondMessageType"),
    generatedCases*
  )

  private def notificationType(term: String): NotificationType = term match
    case "info"    => Info
    case "error"   => Error
    case "success" => Success
    case other     => throw IllegalArgumentException(s"Unknown notification type: $other")

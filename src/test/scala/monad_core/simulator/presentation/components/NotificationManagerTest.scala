package monad_core.simulator.presentation.components

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class NotificationManagerTest extends AnyFunSuite with Matchers:

  test("getNotificationColor should return the correct color for each notification type"):
    val cases = Table(
      ("notifType", "expectedColor"),
      (Info, "#333333"),
      (Success, "#2e7d32"),
      (Error, "#c62828")
    )

    forAll(cases): (notifType, expectedColor) =>
      NotificationManager.getNotificationColor(notifType) should be(expectedColor)

  test("getNotificationColor should default to Info's color when no notification type is given"):
    NotificationManager.getNotificationColor() should be("#333333")

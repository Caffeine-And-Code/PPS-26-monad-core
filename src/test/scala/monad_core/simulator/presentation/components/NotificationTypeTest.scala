package monad_core.simulator.presentation.components

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NotificationTypeTest extends AnyFunSuite with Matchers:

  test("Notification should expose the given message and severity"):
    val notification = Notification("Hello", Success)

    notification.message should be("Hello")
    notification.severity should be(Success)

  test("Two Notifications with the same message and severity should be equal"):
    Notification("Hello", Error) should be(Notification("Hello", Error))

  test("Two Notifications with different message or severity should not be equal"):
    Notification("Hello", Error) should not be Notification("Hello", Success)
    Notification("Hello", Error) should not be Notification("Bye", Error)

package monad_core.simulator.presentation.components

import scalafx.animation.{FadeTransition, ParallelTransition, PauseTransition, TranslateTransition}
import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane
import scalafx.util.Duration

import scala.collection.immutable.Queue

sealed trait NotificationType

case object Info extends NotificationType

case object Success extends NotificationType

case object Error extends NotificationType

case class Notification(
    message: String,
    severity: NotificationType
)

object NotificationManager:
  private val NotificationHeight             = 40
  private val PaddingTopBetweenNotifications = 10

  private var notifications: Queue[Notification] = Queue.empty
  private var overlay: Option[StackPane]         = None
  var animationsEnabled: Boolean                 = true

  def attach(rootOverlay: StackPane): Unit =
    overlay = Some(rootOverlay)

  def detach(): Unit =
    overlay = None
    notifications = Queue.empty

  private[components] def getNotificationColor(severity: NotificationType = Info): String =
    severity match
      case Info    => "#333333"
      case Success => "#2e7d32"
      case Error   => "#c62828"

  def show(message: String, severity: NotificationType = Info): Unit =
    overlay.foreach { root =>
      val bgColor            = getNotificationColor(severity)
      val notificationNumber = notifications.length
      val notificationPosition =
        NotificationHeight * notificationNumber + PaddingTopBetweenNotifications * notificationNumber

      val snackbar = new Label(message) {
        style = s"""
          -fx-background-color: $bgColor;
          -fx-text-fill: white;
          -fx-padding: 12 20 12 20;
          -fx-background-radius: 6;
          -fx-font-size: 13px;
        """
        // Se le animazioni sono disabilitate, imposta subito l'opacità a 1
        opacity = if (animationsEnabled) 0 else 1
        maxWidth = 400
        prefHeight = NotificationHeight
        minHeight = NotificationHeight
        wrapText = true
      }

      StackPane.setAlignment(snackbar, scalafx.geometry.Pos.TopRight)
      snackbar.translateY = if (animationsEnabled) 40 else notificationPosition
      snackbar.margin = scalafx.geometry.Insets(PaddingTopBetweenNotifications, 0, 30, 0)

      root.children.add(snackbar)
      notifications = notifications.enqueue(Notification(message, severity))

      if animationsEnabled then
        val fadeIn = new FadeTransition(Duration(300), snackbar) {
          fromValue = 0
          toValue = 1
        }

        val slideIn = new TranslateTransition(Duration(200), snackbar) {
          fromY = 40
          toY = notificationPosition
        }

        val entrance = new ParallelTransition() {
          children = Seq(fadeIn, slideIn)
        }

        val pause = new PauseTransition(Duration(2500))

        val fadeOut = new FadeTransition(Duration(400), snackbar) {
          fromValue = 1
          toValue = 0
          onFinished = _ => root.children.remove(snackbar)
        }

        entrance.play()
        entrance.onFinished = _ =>
          pause.play()
          pause.onFinished = _ =>
            fadeOut.play()
            notifications = notifications.dequeue._2
    }

package monad_core.simulator.presentation.components

import scalafx.animation.{FadeTransition, ParallelTransition, PauseTransition, TranslateTransition}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane
import scalafx.util.Duration

import scala.collection.immutable.Queue

/** Visual category used to distinguish notifications. */
sealed trait NotificationType

/** Neutral informational notification. */
case object Info extends NotificationType

/** Notification reporting a successful operation. */
case object Success extends NotificationType

/** Notification reporting a failed operation. */
case object Error extends NotificationType

/**
 * Message displayed by the notification manager.
 *
 * @param message
 *   text shown to the user
 * @param severity
 *   visual category of the notification
 */
case class Notification(
    message: String,
    severity: NotificationType
)

/**
 * Manages transient snackbar notifications on a JavaFX overlay.
 *
 * An overlay must first be registered with `attach`. Notifications are stacked in the top-right corner and, when
 * animations are enabled, are automatically dismissed after their entrance animation and display interval. Calling
 * `show` without an attached overlay has no effect.
 */
object NotificationManager:

  private val NotificationHeight             = 40
  private val PaddingTopBetweenNotifications = 10

  /**
   * Controls notification entrance and dismissal animations.
   *
   * When disabled, new notifications are displayed immediately and automatic dismissal is skipped.
   */
  var animationsEnabled: Boolean = true

  private case class NotificationManagerState(
      notifications: Queue[Notification] = Queue.empty,
      overlay: Option[StackPane] = None
  )

  extension (s: NotificationManagerState)

    private def withOverlay(root: StackPane): NotificationManagerState =
      s.copy(overlay = Some(root))

    private def cleared: NotificationManagerState =
      s.copy(overlay = None, notifications = Queue.empty)

    private def notified(notification: Notification): NotificationManagerState =
      s.copy(notifications = s.notifications.enqueue(notification))

    private def dismissedOldest: NotificationManagerState =
      if s.notifications.isEmpty then s
      else s.copy(notifications = s.notifications.dequeue._2)

  private var state: NotificationManagerState = NotificationManagerState()

  /**
   * Registers the overlay on which subsequent notifications are displayed.
   *
   * @param rootOverlay
   *   container that receives notification nodes
   */
  def attach(rootOverlay: StackPane): Unit =
    state = state.withOverlay(rootOverlay)

  /**
   * Detaches the current overlay and clears the internal notification queue.
   *
   * Nodes already added to the previous overlay are not removed.
   */
  def detach(): Unit =
    state = state.cleared

  /**
   * Returns the CSS background color associated with a notification category.
   *
   * @param severity
   *   notification category; defaults to `Info`
   * @return
   *   color encoded as a CSS hexadecimal string
   */
  private[components] def getNotificationColor(severity: NotificationType = Info): String =
    severity match
      case Info    => "#333333"
      case Success => "#2e7d32"
      case Error   => "#c62828"

  private def notificationPosition(index: Int): Double =
    NotificationHeight * index + PaddingTopBetweenNotifications * index

  /**
   * Displays a notification on the attached overlay.
   *
   * If no overlay is attached, the call has no effect.
   *
   * @param message
   *   text shown to the user
   * @param severity
   *   visual category of the notification; defaults to `Info`
   */
  def show(message: String, severity: NotificationType = Info): Unit =
    state.overlay.foreach { root =>
      val notification = Notification(message, severity)
      val position     = notificationPosition(state.notifications.length)
      val snackbar     = buildSnackbar(notification, position)

      root.children.add(snackbar)
      state = state.notified(notification)

      if animationsEnabled then playEntranceThenDismiss(snackbar, root, position)
    }

  private def buildSnackbar(notification: Notification, position: Double): Label =
    val bgColor = getNotificationColor(notification.severity)
    val snackbar = new Label(notification.message) {
      style = s"""
        -fx-background-color: $bgColor;
        -fx-text-fill: white;
        -fx-padding: 12 20 12 20;
        -fx-background-radius: 6;
        -fx-font-size: 13px;
      """
      // Se le animazioni sono disabilitate, imposta subito l'opacità a 1
      opacity = if animationsEnabled then 0 else 1
      maxWidth = 400
      prefHeight = NotificationHeight
      minHeight = NotificationHeight
      wrapText = true
    }
    StackPane.setAlignment(snackbar, Pos.TopRight)
    snackbar.translateY = if animationsEnabled then 40 else position
    snackbar.margin = Insets(PaddingTopBetweenNotifications, 0, 30, 0)
    snackbar

  private def playEntranceThenDismiss(snackbar: Label, root: StackPane, position: Double): Unit =
    val fadeIn = new FadeTransition(Duration(300), snackbar) {
      fromValue = 0
      toValue = 1
    }
    val slideIn = new TranslateTransition(Duration(200), snackbar) {
      fromY = 40
      toY = position
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
        state = state.dismissedOldest

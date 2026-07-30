package integrations.monad_core.simulator.presentation.components

import integrations.monad_core.simulator.presentation.support.{ScalaFxInit, SnapshotTesting}
import monad_core.simulator.presentation.components.{Info, NotificationManager, Success, Error}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane
import scalafx.stage.Stage

import scala.compiletime.uninitialized

class NotificationManagerTest extends AnyFunSuite with Matchers with ScalaFxInit with SnapshotTesting with BeforeAndAfterEach:

  private var root: StackPane = uninitialized
  private var stage: Stage = uninitialized

  override def beforeEach(): Unit =
    runOnFxThread {
      root = new StackPane() {
        prefWidth = 300
        prefHeight = 300
      }

      stage = new Stage() {
        scene = new scalafx.scene.Scene(root)
      }

      NotificationManager.attach(root)
      NotificationManager.animationsEnabled = true
    }
    super.beforeEach()

  override def afterEach(): Unit =
    runOnFxThread {
      NotificationManager.detach()
    }
    super.afterEach()

  private def resetManagerAndScene(): Unit =
    NotificationManager.detach()
    root.children.clear()
    NotificationManager.attach(root)

  private def labelsInRoot: List[Label] =
    root.children.collect { case l: javafx.scene.control.Label => new Label(l) }.toList

  test("show should do nothing when no overlay has been attached"):
    runOnFxThread {
      NotificationManager.detach()

      noException should be thrownBy NotificationManager.show("Hello")
    }

  test("show should add a label with the given message to the attached overlay"):
    runOnFxThread {
      NotificationManager.show("Hello world")

      val labels = labelsInRoot
      labels should have size 1
      labels.head.text.value should be("Hello world")
    }

  test("show should support multiple notifications stacking in the overlay"):
    runOnFxThread {
      NotificationManager.show("First")
      NotificationManager.show("Second")

      labelsInRoot.map(_.text.value) should be(Seq("First", "Second"))
    }

  test("show should apply the correct background color for each notification type"):
    val cases = Table(
      ("notifType", "expectedColor"),
      (Info, "#333333"),
      (Success, "#2e7d32"),
      (Error, "#c62828")
    )

    forAll(cases): (notifType, expectedColor) =>
      runOnFxThread {
        root.children.clear()

        NotificationManager.show("Message", notifType)

        labelsInRoot.head.style.value should include(expectedColor)
      }

  test("show should default to Info type when no notification type is given"):
    runOnFxThread {
      NotificationManager.show("Default type message")

      labelsInRoot.head.style.value should include("#333333")
    }

  test("show should start the label with a wrapped max width of 400 and initial opacity of 0"):
    val expectedMaxWidth: Double = 400.0
    val expectedWrapText: Boolean = true
    val expectedOpacity: Double = 0.0

    var label: Option[Label] = None
    runOnFxThread {
      NotificationManager.show("Some message")

      label = Some(labelsInRoot.head)
    }

    label.get.maxWidth.value should be(expectedMaxWidth)
    label.get.wrapText.value should be(expectedWrapText)
    label.get.opacity.value should be(expectedOpacity)

  test("show should position the notification at the top right with the expected initial offset"):
    var label: Option[Label] = None

    runOnFxThread {
      NotificationManager.show("Positioned message")

      label = Some(labelsInRoot.head)
    }

    StackPane.getAlignment(label.get) should be(scalafx.geometry.Pos.TopRight)
    label.get.translateY.value should be(40.0 +- 0.5)

  test("Info notification matches visual snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Info message")
    }

    assertMatchesVisualSnapshot("info_notification_snapshot", root, maxDiffPercentage = 0.2)

  test("Error notification matches visual snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Error message", Error)
    }

    assertMatchesVisualSnapshot("error_notification_snapshot", root, maxDiffPercentage = 0.2)

  test("Success notification matches visual snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Success message", Success)
    }

    assertMatchesVisualSnapshot("success_notification_snapshot", root, maxDiffPercentage = 0.2)

  test("Info notification matches architectural snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Info message")
    }

    assertMatchesArchitecturalSnapshotOfStage("info_notification_snapshot", stage)

  test("Error notification matches architectural snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Error message", Error)
    }

    assertMatchesArchitecturalSnapshotOfStage("error_notification_snapshot", stage)

  test("Success notification matches architectural snapshot"):
    runOnFxThread {
      NotificationManager.animationsEnabled = false
      NotificationManager.show("Positioned Success message", Success)
    }

    assertMatchesArchitecturalSnapshotOfStage("success_notification_snapshot", stage)

  //TODO: this will probably make a good usage of Prolog
  test("Notification Stacks matches the architectural snapshot"):
    val cases = Table(
      ("firstMessageType", "secondMessageType"),
      (Info, Info),
      (Info, Error),
      (Info, Success),
      (Error, Info),
      (Error, Error),
      (Error, Success),
      (Success, Info),
      (Success, Error),
      (Success, Success),
    )

    forAll(cases): (firstMessageType, secondMessageType) =>
      runOnFxThread {
        resetManagerAndScene()

        NotificationManager.animationsEnabled = false
        NotificationManager.show(s"Positioned $firstMessageType message", firstMessageType)
        NotificationManager.show(s"Positioned $secondMessageType message", secondMessageType)
      }

      assertMatchesArchitecturalSnapshotOfStage(s"${firstMessageType}_${secondMessageType}_notification_snapshot", stage)

  //TODO: this will probably make a good usage of Prolog
  test("Notification Stacks matches the visual snapshot"):
    val cases = Table(
      ("firstMessageType", "secondMessageType"),
      (Info, Info),
      (Info, Error),
      (Info, Success),
      (Error, Info),
      (Error, Error),
      (Error, Success),
      (Success, Info),
      (Success, Error),
      (Success, Success),
    )

    forAll(cases): (firstMessageType, secondMessageType) =>
      runOnFxThread {
        resetManagerAndScene()

        NotificationManager.animationsEnabled = false
        NotificationManager.show(s"Positioned $firstMessageType message", firstMessageType)
        NotificationManager.show(s"Positioned $secondMessageType message", secondMessageType)
      }

      assertMatchesVisualSnapshot(s"${firstMessageType}_${secondMessageType}_notification_snapshot", root, maxDiffPercentage = 0.6)
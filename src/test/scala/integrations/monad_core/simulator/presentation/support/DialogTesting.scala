package integrations.monad_core.simulator.presentation.support

import integrations.monad_core.simulator.presentation.support.FxThreadHelper.onFxThread
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters.ListHasAsScala

trait DialogTesting
    extends AnyFunSuite
    with BeforeAndAfterEach
    with ScalaFxInit
    with SnapshotTesting:

  override def beforeEach(): Unit =
    if getActiveStage.isDefined then
      onFxThread {
        getRequiredActiveStage.close()

        javafx.stage.Window.getWindows.asScala.toList
          .collect { case s: javafx.stage.Stage => s }
          .foreach(_.close())
      }
    super.beforeEach()

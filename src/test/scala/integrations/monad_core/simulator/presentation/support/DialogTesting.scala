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

  private def closeAllWindows(): Unit =
    onFxThread {
      javafx.stage.Window.getWindows.asScala.toList.foreach(_.hide())
    }

  override def beforeEach(): Unit =
    closeAllWindows()
    super.beforeEach()

  override def afterEach(): Unit =
    try closeAllWindows()
    finally super.afterEach()

package integrations.monad_core.simulator.presentation.support

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

trait DialogTesting extends AnyFunSuite with BeforeAndAfterEach with ScalaFxInit with SnapshotTesting:

  override def beforeEach(): Unit =
    if getActiveStage.isDefined then
      runOnFxThread{
        getRequiredActiveStage.close()
      }
    super.beforeEach()
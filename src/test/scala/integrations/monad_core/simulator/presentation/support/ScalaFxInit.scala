package integrations.monad_core.simulator.presentation.support

import org.scalatest.{BeforeAndAfterAll, Suite}

private[presentation] trait ScalaFxInit extends BeforeAndAfterAll:

  this: Suite =>
  override def beforeAll(): Unit = {
    ScalaFxToolkit.init()
    super.beforeAll()
  }

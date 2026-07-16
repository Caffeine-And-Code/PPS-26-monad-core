package monad_core.graphics.support

import org.scalatest.{BeforeAndAfterAll, Suite}

private[graphics] trait ScalaFxInit extends BeforeAndAfterAll:

  this: Suite =>
  override def beforeAll(): Unit = {
    ScalaFxToolkit.init()
    super.beforeAll()
  }

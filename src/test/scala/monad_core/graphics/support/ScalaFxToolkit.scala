package monad_core.graphics.support

import javafx.application.Platform

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean
import java.util.logging.{Level, Logger}

/**
 * Initializes the Java/Scala toolkit to enable the test suite to tests
 * UI components, scenes and stages.
 */
private object ScalaFxToolkit {
  private val initialized = new AtomicBoolean(false)

  def init(): Unit = {
    if (initialized.compareAndSet(false, true)) {
      // disable scalaFx warnings for tests
      Logger.getLogger("com.sun.javafx.application.PlatformImpl").setLevel(Level.SEVERE)

      val latch = new CountDownLatch(1)
      Platform.startup(() => latch.countDown())
      latch.await()
      Platform.setImplicitExit(false)
    }
  }
}

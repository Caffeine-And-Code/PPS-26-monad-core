package integrations.monad_core.simulator.presentation.support

import javafx.application.Platform

import java.util.concurrent.{CountDownLatch, TimeUnit}

object FxThreadHelper:

  def onFxThread[A](action: => A): A =
    if Platform.isFxApplicationThread then action
    else
      val completed                              = new CountDownLatch(1)
      @volatile var result: Either[Throwable, A] = null
      Platform.runLater { () =>
        result =
          try Right(action)
          catch case error: Throwable => Left(error)
        completed.countDown()
      }
      completed.await(5L, TimeUnit.SECONDS)
      result.fold(throw _, identity)

package monad_core.simulator.infrastructure.logging

import monad_core.simulator.application.logging.Logger

object ConsoleLogger extends Logger:

  override def info(message: String): Unit =
    Console.println(message)

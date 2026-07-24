package monad_core.simulator.presentation.support

trait ComponentActions[A] {
    def attach(callback: A => Unit): Unit
}

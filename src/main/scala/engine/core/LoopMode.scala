package engine.core

sealed trait LoopMode

case object EditMode extends LoopMode

case object SimulationMode extends LoopMode
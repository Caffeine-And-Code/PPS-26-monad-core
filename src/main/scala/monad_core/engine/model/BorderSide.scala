package monad_core.engine.model

/** Identifies one of the four boundaries of the simulation world. */
enum BorderSide:
  /** Left vertical boundary. */
  case Left

  /** Right vertical boundary. */
  case Right

  /** Upper horizontal boundary. */
  case Top

  /** Lower horizontal boundary. */
  case Bottom

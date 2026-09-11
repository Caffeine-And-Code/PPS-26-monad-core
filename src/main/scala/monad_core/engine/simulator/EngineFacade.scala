package monad_core.engine.simulator

import monad_core.engine.core.events.EngineEvent
import monad_core.engine.core.traits.State
import monad_core.engine.core.{GameLoop, LoopMode}
import monad_core.engine.model.EngineError
import monad_core.engine.physics.core.PhysicsManager

/**
 * Functional public facade for creating, controlling and advancing engine sessions.
 *
 * The facade does not retain global state: every transition receives a session and returns its successor.
 */
object EngineFacade:

  /** Opaque engine session whose underlying game-loop representation is hidden from clients. */
  opaque type Session = GameLoop

  /** Default duration of a fixed update, in nanoseconds. */
  val DefaultTickTime: Long = GameLoop.DefaultTickTime

  /** Default maximum amount of elapsed time processed by one tick, in nanoseconds. */
  val DefaultMaxFrameTime: Long = GameLoop.DefaultMaxFrameTime

  /**
   * Result of successfully advancing an engine session.
   *
   * @param state
   *   most recent state produced by fixed updates
   * @param previousState
   *   state preceding `state`, used as the interpolation origin
   * @param nextSession
   *   session carrying updated timing information
   * @param events
   *   events accumulated during fixed updates
   * @param alpha
   *   interpolation coefficient derived from time remaining in the accumulator
   */
  final case class TickResult(
      state: State,
      previousState: State,
      nextSession: Session,
      events: Vector[EngineEvent],
      alpha: Double
  )

  /**
   * Public status of a configurable physics rule.
   *
   * @param id
   *   stable rule identifier
   * @param isEnabled
   *   whether the rule participates in physics updates
   */
  final case class PhysicsRuleStatus(id: String, isEnabled: Boolean)

  /** @return a stopped session in edit mode using default timing values */
  def default: Session =
    GameLoop.default()

  /**
   * Creates a stopped session in edit mode with custom timing values.
   *
   * @param tickTime
   *   fixed-update duration in nanoseconds
   * @param maxFrameTime
   *   maximum elapsed time processed by one tick in nanoseconds
   * @return
   *   a session, or an engine error when the timing configuration is invalid
   */
  def create(
      tickTime: Long = DefaultTickTime,
      maxFrameTime: Long = DefaultMaxFrameTime
  ): Either[EngineError, Session] =
    GameLoop(tickTime = tickTime, maxFrameTime = maxFrameTime)

  /**
   * Starts a session and switches it to simulation mode.
   *
   * @param session
   *   session to start
   * @return
   *   the updated session
   */
  def start(session: Session): Session =
    session.start()

  /**
   * Stops a session and switches it to edit mode.
   *
   * @param session
   *   session to stop
   * @return
   *   the updated session
   */
  def stop(session: Session): Session =
    session.stop()

  /**
   * Returns the current mode of a session.
   *
   * @param session
   *   session to inspect
   * @return
   *   its current loop mode
   */
  def mode(session: Session): LoopMode =
    session.mode

  /**
   * Reports whether a session is running.
   *
   * @param session
   *   session to inspect
   * @return
   *   `true` when it is running, otherwise `false`
   */
  def isRunning(session: Session): Boolean =
    session.isRunning

  /**
   * Returns the configured rules and their enabled state.
   *
   * @param physics
   *   physics manager to inspect
   * @return
   *   rule statuses in manager configuration order
   */
  def physicsRules(physics: PhysicsManager): Vector[PhysicsRuleStatus] =
    physics.rules.toVector.map(rule => PhysicsRuleStatus(rule.RuleId, physics.isEnabled(rule)))

  /**
   * Enables or disables a physics rule by identifier.
   *
   * An unknown identifier leaves the manager unchanged.
   *
   * @param physics
   *   physics manager to update
   * @param ruleId
   *   identifier of the target rule
   * @param isEnabled
   *   desired enabled state
   * @return
   *   the updated immutable manager, or the original manager when the rule is unknown
   */
  def setPhysicsRuleEnabled(
      physics: PhysicsManager,
      ruleId: String,
      isEnabled: Boolean
  ): PhysicsManager =
    physics.rules
      .find(_.RuleId == ruleId)
      .fold(physics)(rule => if isEnabled then physics.enable(rule) else physics.disable(rule))

  /**
   * Advances a session to the supplied time.
   *
   * A stopped or edit-mode session leaves the state unchanged but still advances its recorded time. A running session
   * executes as many fixed updates as fit in the accumulated elapsed time.
   *
   * @param session
   *   current engine session
   * @param state
   *   current world state
   * @param currentTime
   *   current monotonic time in nanoseconds
   * @param physics
   *   physics manager used by fixed updates
   * @return
   *   the updated state and session data, or the first engine error
   */
  def tick(
      session: Session,
      state: State,
      currentTime: Long,
      physics: PhysicsManager = PhysicsManager.default()
  ): Either[EngineError, TickResult] =
    given PhysicsManager = physics

    session.tick(state, currentTime).map { result =>
      TickResult(
        state = result.state,
        previousState = result.previousState,
        nextSession = result.loop,
        events = result.events,
        alpha = result.alpha
      )
    }

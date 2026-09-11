package monad_core.engine.model

/** Element that has an identity, position, shape, and rotation in the simulation world. */
trait Locatable:
  /** @return unique identifier of this element */
  def id: LocatableId

  /** @return position of this element in world coordinates */
  def position: Vector2D

  /** @return geometric shape occupied by this element */
  def shape: Shape2D

  /** @return rotation in degrees in the inclusive range `[0, 360]` */
  def rotation: Double

/** Validating construction operations. */
object Locatable:

  /**
   * Validates circle data and builds a locatable value.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param radius strictly positive circle radius
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @param build constructor invoked with the validated identifier, position, shape, and rotation
   * @tparam A locatable type produced by `build`
   * @return the constructed value, or the first identifier, shape, position, or rotation validation error
   */
  def circle[A](id: String, position: Vector2D, radius: Double, rotation: Double = 0)(
      build: (LocatableId, Vector2D, Shape2D, Double) => A
  ): Either[EngineError, A] =
    Shape2D.circle(radius).flatMap(circle => createGeneric(id, position, circle, rotation)(build))

  private def createGeneric[A](
      id: String,
      position: Vector2D,
      shape: Shape2D,
      rotation: Double
  )(build: (LocatableId, Vector2D, Shape2D, Double) => A): Either[EngineError, A] =
    for {
      locatableId <- LocatableId(id)
      _           <- validate(position, rotation)
    } yield build(locatableId, position, shape, rotation)

  private[model] def validateAndReturn[L <: Locatable](updated: L): Either[EngineError, L] =
    validate(updated.position, updated.rotation).map(_ => updated)

  private def validate(position: Vector2D, rotation: Double): Either[EngineError, Unit] =
    for
      _ <- validatePosition(position)
      _ <- validateRotation(rotation)
    yield ()

  private def validatePosition(position: Vector2D): Either[EngineError, Unit] =
    Either.cond(position.x >= 0 && position.y >= 0, (), PositionIsValid(position))

  /**
   * Validates a rotation.
   *
   * @param rotation rotation in degrees
   * @return `Right(())` when the rotation is in `[0, 360]`, or [[RotationMustBeAValidDegreeValue]] otherwise
   */
  private[model] def validateRotation(rotation: Double): Either[EngineError, Unit] =
    Either.cond(rotation >= 0 && rotation <= 360, (), RotationMustBeAValidDegreeValue(rotation))

  /**
   * Validates rectangle data and builds a locatable value.
   *
   * @param id raw non-empty identifier
   * @param position position with non-negative coordinates
   * @param height strictly positive rectangle height
   * @param length strictly positive rectangle length
   * @param rotation rotation in degrees in the inclusive range `[0, 360]`
   * @param build constructor invoked with the validated identifier, position, shape, and rotation
   * @tparam A locatable type produced by `build`
   * @return the constructed value, or the first identifier, shape, position, or rotation validation error
   */
  def rectangle[A](
      id: String,
      position: Vector2D,
      height: Double,
      length: Double,
      rotation: Double = 0
  )(build: (LocatableId, Vector2D, Shape2D, Double) => A): Either[EngineError, A] =
    Shape2D
      .rectangle(height, length)
      .flatMap(rectangle => createGeneric(id, position, rectangle, rotation)(build))

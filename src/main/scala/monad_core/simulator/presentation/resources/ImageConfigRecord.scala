package monad_core.simulator.presentation.resources

/**
 * Configuration record that provides to the Image system the location information regarding the images.
 * 
 * @see [[ImageLoader]]
 */
trait ImageConfigRecord(val imageBasePath: String)

/**
 * Default implementation of [[ImageConfigRecord]] 
 */
case class BaseImageConfig() extends ImageConfigRecord(imageBasePath = "/images/")

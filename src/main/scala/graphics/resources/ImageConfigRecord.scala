package graphics.resources

trait ImageConfigRecord(val imageBasePath : String)

case class BaseImageConfig() extends ImageConfigRecord(imageBasePath = "/") 

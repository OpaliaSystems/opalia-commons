package systems.opalia.commons.scripting.ejs

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source


trait EjsConfiguration {

  val openWith: String
  val closeWith: String

  def resolve(path: Path)(implicit executor: ExecutionContext): Future[Source]
}

object EjsDefaultConfiguration
  extends EjsConfiguration {

  val openWith: String = "<%"
  val closeWith: String = "%>"

  def resolve(path: Path)
             (implicit executor: ExecutionContext): Future[Source] =
    Future(Source.fromFile(path.toFile))
}

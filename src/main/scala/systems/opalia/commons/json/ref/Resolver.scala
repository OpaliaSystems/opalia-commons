package systems.opalia.commons.json.ref

import java.nio.file.{Files, Paths}
import play.api.libs.json._


trait Resolver {

  def resolve(currentUri: String, previousUri: String): String

  def load(uri: String): RefTransformer.Document
}

// The default resolution only supports JSON on filesystem.
// It is basically up to the user to implement support for YAML or support for fetching network resources.
object DefaultResolver
  extends Resolver {

  def resolve(currentUri: String, previousUri: String): String =
    Paths.get(previousUri).resolveSibling(currentUri).normalize.toString

  def load(uri: String): RefTransformer.Document =
    RefTransformer.Document(uri, Json.parse(Files.readAllBytes(Paths.get(uri))))
}

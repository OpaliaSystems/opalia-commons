package systems.opalia.commons.json.format

import java.nio.file.Path
import java.time.{OffsetDateTime, OffsetTime}
import play.api.libs.json._
import scala.language.implicitConversions
import scala.util.matching.Regex
import systems.opalia.commons.identifier.{ObjectId, UniversallyUniqueId}
import systems.opalia.commons.net.{EndpointAddress, Uri}


object Imports {

  implicit val formatBigInt = new BigIntFormat()
  implicit val formatUri = new UriFormat()
  implicit val formatPath = new PathFormat()
  implicit val formatOffsetDateTime = new OffsetDateTimeFormat()
  implicit val formatOffsetTime = new OffsetTimeFormat()
  implicit val formatEndpointAddress = new EndpointAddressFormat()
  implicit val formatObjectId = new ObjectIdFormat()
  implicit val formatUniversallyUniqueId = new UniversallyUniqueIdFormat()
  implicit val formatRegex = new RegexFormat()

  implicit def wrapBigInt(x: BigInt) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapUri(x: Uri) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapPath(x: Path) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapOffsetDateTime(x: OffsetDateTime) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapOffsetTime(x: OffsetTime) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapEndpointAddress(x: EndpointAddress) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapObjectId(x: ObjectId) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapUniversallyUniqueId(x: UniversallyUniqueId) =
    Json.toJsFieldJsValueWrapper(x)

  implicit def wrapRegex(x: Regex) =
    Json.toJsFieldJsValueWrapper(x)
}

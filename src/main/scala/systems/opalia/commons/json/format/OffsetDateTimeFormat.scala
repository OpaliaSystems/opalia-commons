package systems.opalia.commons.json.format

import java.time.OffsetDateTime
import play.api.libs.json._
import scala.util.{Failure, Success, Try}


class OffsetDateTimeFormat
  extends Format[OffsetDateTime] {

  override def reads(node: JsValue): JsResult[OffsetDateTime] =
    node match {
      case JsString(x) =>
        Try(OffsetDateTime.parse(x)) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.OffsetDateTime (ISO8601)\n" + e.getMessage)
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: OffsetDateTime): JsValue =
    JsString(x.toString)
}

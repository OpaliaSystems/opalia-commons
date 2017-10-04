package systems.opalia.commons.json.format

import java.time.OffsetTime
import play.api.libs.json._
import scala.util.{Failure, Success, Try}


class OffsetTimeFormat
  extends Format[OffsetTime] {

  override def reads(node: JsValue): JsResult[OffsetTime] =
    node match {
      case JsString(x) =>
        Try(OffsetTime.parse(x)) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.OffsetTime (ISO8601)\n" + e.getMessage)
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: OffsetTime): JsValue =
    JsString(x.toString)
}

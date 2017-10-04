package systems.opalia.commons.json.format

import play.api.libs.json._
import scala.util.{Failure, Success, Try}
import systems.opalia.commons.net.Uri


class UriFormat
  extends Format[Uri] {

  override def reads(node: JsValue): JsResult[Uri] =
    node match {
      case JsString(x) =>
        Try(Uri(x)) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.Uri (URI)\n" + e.getMessage)
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: Uri): JsValue =
    JsString(x.toString)
}

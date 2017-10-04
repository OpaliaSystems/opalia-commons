package systems.opalia.commons.json.format

import play.api.libs.json._
import scala.util.{Failure, Success, Try}
import systems.opalia.commons.net.EndpointAddress


class EndpointAddressFormat
  extends Format[EndpointAddress] {

  override def reads(node: JsValue): JsResult[EndpointAddress] =
    node match {
      case JsString(x) =>
        Try(EndpointAddress.parse(x)) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.EndpointAddress\n" + e.getMessage)
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: EndpointAddress): JsValue =
    JsString(x.toString)
}

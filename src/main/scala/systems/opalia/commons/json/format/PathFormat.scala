package systems.opalia.commons.json.format

import java.nio.file.{Path, Paths}
import play.api.libs.json._
import scala.util.{Failure, Success, Try}


class PathFormat
  extends Format[Path] {

  override def reads(node: JsValue): JsResult[Path] =
    node match {
      case JsString(x) =>
        Try(Paths.get(x)) match {
          case Success(v) => JsSuccess(v)
          case Failure(e) => JsError("error.expected.format.Path")
        }
      case _ => JsError("error.expected.jsstring")
    }

  def writes(x: Path): JsValue =
    JsString(x.toString)
}

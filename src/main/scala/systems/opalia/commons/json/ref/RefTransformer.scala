package systems.opalia.commons.json.ref

import play.api.libs.json._
import scala.collection.immutable.ListMap
import scala.collection.mutable
import systems.opalia.commons.net.UriHelper


class RefTransformer private(resolver: Resolver) {

  // https://json-schema.org/understanding-json-schema/structuring.html
  // http://spec.openapis.org/oas/v3.0.3#reference-object
  // https://swagger.io/docs/specification/using-ref/

  def transform(uri: String): JsValue =
    transform(resolver.load(uri))

  def transform(document: RefTransformer.Document): JsValue = {

    val nodePool = mutable.HashMap[String, JsObject]()

    val node =
      try {

        val result1 = document.replaceNode(x => setUriField(x.node, None))
        val result2 = document.withNode(extractIdFields(result1, nodePool))
        val result3 = document.withNode(replaceIdReferences(result2, nodePool))
        replaceRegularReferences(result3)

      } finally {

        nodePool.clear()
      }

    node
  }

  private def extractIdFields(document: RefTransformer.Document,
                              nodePool: mutable.HashMap[String, JsObject]): JsValue = {

    def extractIdField(node: JsValue): JsValue =
      node match {

        case n: JsObject => {

          val m = JsObject(ListMap(n.fields.filter(_._1 != "$id").map(x => x._1 -> extractIdField(x._2)): _*))

          n.fields.find(_._1 == "$id").foreach(x => safeNodeToPool(x._2, m, document, nodePool))

          m
        }
        case x: JsArray =>
          JsArray(x.value.map(extractIdField))
        case other =>
          other
      }

    extractIdField(document.node)
  }

  private def replaceIdReferences(document: RefTransformer.Document,
                                  nodePool: mutable.HashMap[String, JsObject]): JsValue = {

    def replaceIdReferences(node: JsValue): JsValue =
      node match {

        case parent: JsObject =>
          parent.fields.find(_._1 == "$ref")
            .flatMap(x => resolveIdReferences(x._2, document, nodePool))
            .getOrElse(JsObject(ListMap(parent.fields.map(x => x._1 -> replaceIdReferences(x._2)): _*)))
        case x: JsArray =>
          JsArray(x.value.map(replaceIdReferences))
        case other =>
          other
      }

    replaceIdReferences(document.node)
  }

  private def replaceRegularReferences(document: RefTransformer.Document): JsValue = {

    def replaceRegularReferences(node: JsValue): JsValue =
      node match {

        case parent: JsObject =>
          parent.fields.find(_._1 == "$ref")
            .map {
              case (_, refNode) =>

                val correctedDocument =
                  parent.fields.flatMap {
                    case ("$uri", JsString(uri)) => Some(uri)
                    case _ => None
                  }
                    .headOption
                    .map(x => document.withUri(x))
                    .getOrElse(document)

                resolveRegularReferences(refNode, correctedDocument)
            }
            .getOrElse(JsObject(ListMap(parent.fields.map(x => x._1 -> replaceRegularReferences(x._2)): _*)))
        case parent: JsArray =>
          JsArray(parent.value.map(replaceRegularReferences))
        case other =>
          other
      }

    var node: JsValue = document.node
    var nodeTemp: JsValue = null

    while (node != nodeTemp) {

      nodeTemp = node
      node = replaceRegularReferences(node)
    }

    node
  }

  private def resolveIdReferences(node: JsValue,
                                  document: RefTransformer.Document,
                                  nodePool: mutable.HashMap[String, JsObject]): Option[JsValue] =
    node match {

      case JsString(value) if (value.nonEmpty) => {

        val uri = value.takeWhile(_ != '#')
        val fragment = UriHelper.decode(value.drop(uri.length).dropWhile(_ == '#'))
        val searchLocal = uri.isEmpty
        val searchById = !fragment.startsWith("/")

        if (searchById) {

          val idPart = fragment.takeWhile(_ != '/')
          val pointerPart = fragment.drop(idPart.length)
          val id = JsonPointer.decode(idPart)
          val pointer = JsonPointer(pointerPart)

          val currentNode =
            if (searchLocal) {

              val importedObject =
                nodePool.getOrElse(id,
                  throw new IllegalArgumentException(
                    s"[${document.uri}] Cannot find object with $$id field “$id”."))

              replaceIdReferences(document.withNode(importedObject), nodePool)

            } else {

              val importedDocument =
                resolver.load(resolver.resolve(uri, document.uri)).replaceNode(x => setUriField(x.node, Some(x.uri)))

              val importedNodePool = mutable.HashMap[String, JsObject]()

              try {

                extractIdFields(importedDocument, importedNodePool)

                val importedObject =
                  importedNodePool.getOrElse(id,
                    throw new IllegalArgumentException(
                      s"[${importedDocument.uri}] Cannot find object with $$id field “$id”."))

                replaceIdReferences(importedDocument.withNode(importedObject), nodePool)

              } finally {

                importedNodePool.clear()
              }
            }

          try {

            Some(pointer.search(currentNode))

          } catch {
            case e: Exception =>
              throw new IllegalArgumentException(
                s"[${document.uri}] Cannot apply JSON pointer.", e)
          }
        } else
          None
      }
      case _ =>
        throw new IllegalArgumentException(
          s"[${document.uri}] The value of a $$ref field must be a non empty string.")
    }

  private def resolveRegularReferences(node: JsValue, document: RefTransformer.Document): JsValue =
    node match {

      case JsString(value) if (value.nonEmpty) => {

        val uri = value.takeWhile(_ != '#')
        val fragment = UriHelper.decode(value.drop(uri.length).dropWhile(_ == '#'))
        val searchLocal = uri.isEmpty
        val pointer = JsonPointer(fragment)

        val currentNode =
          if (searchLocal)
            document.node
          else
            transform(resolver.resolve(uri, document.uri))

        try {

          pointer.search(currentNode)

        } catch {
          case e: Exception =>
            throw new IllegalArgumentException(
              s"[${document.uri}] Cannot apply JSON pointer.", e)
        }
      }
      case _ =>
        throw new IllegalArgumentException(
          s"[${document.uri}] The value of a $$ref field must be a non empty string.")
    }

  private def safeNodeToPool(idNode: JsValue,
                             node: JsObject,
                             document: RefTransformer.Document,
                             nodePool: mutable.HashMap[String, JsObject]): Unit =
    idNode match {

      case JsString(id) if (id.nonEmpty) => {

        if (nodePool.contains(id))
          throw new IllegalArgumentException(
            s"[${document.uri}] The value of $$id field “$id” is not unique.")

        nodePool.put(id, node)
      }
      case _ =>
        throw new IllegalArgumentException(
          s"[${document.uri}] The value of an $$id field must be a non empty string.")
    }

  private def setUriField(node: JsValue, uri: Option[String]): JsValue =
    node match {

      case parent: JsObject =>
        parent.fields.find(_._1 == "$ref")
          .map(x => JsObject(ListMap(x, "$uri" -> uri.map(JsString(_)).getOrElse(JsNull))))
          .getOrElse(JsObject(ListMap(parent.fields.map(x => x._1 -> setUriField(x._2, uri)): _*)))
      case parent: JsArray =>
        JsArray(parent.value.map(x => setUriField(x, uri)))
      case other =>
        other
    }
}

object RefTransformer {

  def apply(resolver: Resolver = DefaultResolver): RefTransformer =
    new RefTransformer(resolver)

  case class Document(uri: String, node: JsValue) {

    def withUri(uri: String): Document =
      Document(uri, node)

    def withNode(node: JsValue): Document =
      Document(uri, node)

    def replaceNode(f: (Document) => JsValue): Document =
      Document(uri, f(this))
  }

}

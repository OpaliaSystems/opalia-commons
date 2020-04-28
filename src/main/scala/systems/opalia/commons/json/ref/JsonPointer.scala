package systems.opalia.commons.json.ref

import org.parboiled2._
import play.api.libs.json._
import scala.collection.immutable.LinearSeq
import scala.collection.{LinearSeqOptimized, mutable}
import shapeless.HNil
import systems.opalia.interfaces.rendering._


class JsonPointer private(tokens: Seq[String])
  extends LinearSeq[String]
    with LinearSeqOptimized[String, JsonPointer]
    with StringRenderable {

  override def isEmpty: Boolean =
    tokens.isEmpty

  override def head: String =
    tokens.head

  override def tail: JsonPointer =
    new JsonPointer(tokens.tail)

  override def equals(that: Any): Boolean =
    that match {

      case that: JsonPointer if (this.sameElements(that)) => true
      case _ => false
    }

  override def renderString(renderer: StringRenderer): StringRenderer =
    renderer ~ tokens.foldLeft(renderer.newEmpty)((a, b) => a ~ '/' ~ JsonPointer.encode(b))

  override protected def newBuilder: mutable.Builder[String, JsonPointer] =
    new mutable.Builder[String, JsonPointer] {

      private val buffer = mutable.ArrayBuffer.newBuilder[String]

      def +=(elem: String): this.type = {

        buffer += elem
        this
      }

      def clear(): Unit =
        buffer.clear()

      def result(): JsonPointer =
        new JsonPointer(buffer.result())
    }

  def search(node: JsValue): JsValue = {

    @scala.annotation.tailrec
    def search(node: JsValue, tokens: List[String]): JsValue =
      (tokens, node) match {

        case (token :: rest, JsObject(fields)) => {

          val currentNode =
            fields.find(_._1 == token)
              .map(_._2)
              .getOrElse(throw new IllegalArgumentException(
                s"Cannot find key “$token” in object to apply pointer “$this”."))

          search(currentNode, rest)
        }
        case (token :: rest, JsArray(elements)) => {

          val currentNode =
            new JsonPointer.JsonPointerParser(token).`array-index`.run()
              .recover {
                case e: Exception =>
                  throw new IllegalArgumentException(
                    s"Cannot parse index “$token” to apply pointer “$this”.", e)
              }
              .map {
                index =>

                  elements.lift(index)
                    .getOrElse(throw new IllegalArgumentException(
                      s"Cannot find index “$token” in array to apply pointer “$this”."))
              }

          search(currentNode.get, rest)
        }
        case (token :: _, _) =>
          throw new IllegalArgumentException(
            s"Cannot search for token “$token” in atomic JSON value to apply pointer “$this”.")
        case (Nil, oddment) =>
          oddment
      }

    search(node, tokens.toList)
  }
}

object JsonPointer {

  def apply(pointer: String): JsonPointer =
    new JsonPointerParser(pointer).`json-pointer`.run().get

  def encode(data: String): String = {

    @scala.annotation.tailrec
    def encode(chars: List[Char], acc: List[Char]): String =
      chars match {
        case '~' :: rest =>
          encode(rest, acc ++ "~0")
        case '/' :: rest =>
          encode(rest, acc ++ "~1")
        case x :: rest =>
          encode(rest, acc :+ x)
        case Nil =>
          new String(acc.toArray)
      }

    encode(data.toList, Nil)
  }

  def decode(data: String): String = {

    @scala.annotation.tailrec
    def decode(chars: List[Char], acc: List[Char]): String =
      chars match {
        case '~' :: '0' :: rest =>
          decode(rest, acc :+ '~')
        case '~' :: '1' :: rest =>
          decode(rest, acc :+ '/')
        case x :: rest =>
          decode(rest, acc :+ x)
        case Nil =>
          new String(acc.toArray)
      }

    decode(data.toList, Nil)
  }

  private class JsonPointerParser(val input: ParserInput)
    extends Parser {

    // https://tools.ietf.org/html/rfc6901

    val unescaped8Bits =
      CharPredicate('\u0000' to '\u002E') ++
        CharPredicate('\u0030' to '\u007D') ++
        CharPredicate.from(x => Character.isDefined(x) && x >= '\u007F')

    val highSurrogate = CharPredicate.from(Character.isHighSurrogate)
    val lowSurrogate = CharPredicate.from(Character.isLowSurrogate)
    val escapedValue = CharPredicate("01")

    def `json-pointer`: Rule1[JsonPointer] =
      rule {

        zeroOrMore('/' ~ `reference-token`) ~> {
          (tokens: Seq[String]) =>

            new JsonPointer(tokens)
        }
      }

    def `reference-token`: Rule1[String] =
      rule {

        capture(zeroOrMore(`unescaped` | `escaped`)) ~> ((x: String) => decode(x))
      }

    def `array-index`: Rule1[Int] =
      rule {

        capture('0' | CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) ~> ((x: String) => x.toInt)
      }

    def `unescaped`: Rule[HNil, HNil] =
      rule {

        (highSurrogate ~ lowSurrogate) | unescaped8Bits
      }

    def `escaped`: Rule[HNil, HNil] =
      rule {

        '~' ~ escapedValue
      }
  }

}

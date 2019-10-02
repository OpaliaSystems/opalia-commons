package systems.opalia.commons.net

import org.parboiled2._
import scala.util.matching.Regex
import scala.util.{Failure, Success}
import systems.opalia.interfaces.rendering._


object PathMatching {

  class ParameterPath private(private val path: Seq[ParameterPath.Segment])
    extends StringRenderable {

    def collides(that: ParameterPath): Boolean =
      this.path.size == that.path.size &&
        this.path.zip(that.path).forall {
          case (_: ParameterPath.Parameter, _) => true
          case (_, _: ParameterPath.Parameter) => true
          case (ParameterPath.Label(x), ParameterPath.Label(y)) => x == y
        }

    def matches(path: ArgumentPath): Boolean =
      this.path.size == path.size &&
        this.path.zip(path).forall {
          case (ParameterPath.Parameter(_, None), _) => true
          case (ParameterPath.Parameter(_, Some(r)), v) => r.pattern.matcher(v).matches()
          case (ParameterPath.Label(l), v) => l == v
        }

    def arguments(path: ArgumentPath): Map[String, String] =
      this.path.zip(path).toMap.flatMap {
        case (ParameterPath.Parameter(k, _), v) => Some(k -> v)
        case _ => None
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      val segments =
        path.map {
          case ParameterPath.Parameter(k, None) => renderer.newEmpty ~ '?' ~ k
          case ParameterPath.Parameter(k, Some(r)) => renderer.newEmpty ~ '?' ~ k ~ '<' ~ r.regex ~ '>'
          case ParameterPath.Label(l) => renderer.newEmpty ~ l
        }

      renderer ~ '/' ~ renderer.newEmpty.glue(segments, "/")
    }

    override def equals(that: Any): Boolean =
      that match {

        case that: ParameterPath if (this.path.sameElements(that.path)) => true
        case _ => false
      }

    override def hashCode: Int =
      path.hashCode
  }

  object ParameterPath {

    def apply(path: String): ParameterPath = {

      val parser = new PathParser(path)

      parser.`path-params-expression`.run() match {
        case Failure(e: ParseError) =>
          throw new IllegalArgumentException(s"Failed to parse parameter path.\n${parser.formatError(e)}")
        case Failure(e) =>
          throw e
        case Success(x) => {

          val keys =
            x.flatMap {
              case Parameter(key, _) => Some(key)
              case _ => None
            }

          if (keys.size != keys.distinct.size)
            throw new IllegalArgumentException(s"Expect unique keys in parameter path.")

          new ParameterPath(x)
        }
      }
    }

    sealed trait Segment

    case class Label(value: String)
      extends Segment

    case class Parameter(key: String, value: Option[Regex])
      extends Segment

  }

  class ArgumentPath(private val path: Seq[String])
    extends IndexedSeq[String]
      with StringRenderable {

    private val charsSegment = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@"

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encSegment(string: String): String =
        UriHelper.encode(string, charsSegment)

      renderer ~ '/' ~ renderer.newEmpty.glue(path.map(x => renderer.newEmpty ~ encSegment(x)), "/")
    }

    override def apply(index: Int): String =
      path(index)

    override def length: Int =
      path.length

    override def equals(that: Any): Boolean =
      that match {

        case that: ArgumentPath if (this.path.sameElements(that.path)) => true
        case _ => false
      }

    override def hashCode: Int =
      path.hashCode
  }

  object ArgumentPath {

    def apply(path: String): ArgumentPath = {

      val parser = new PathParser(path)

      parser.`path-args-expression`.run() match {
        case Failure(e: ParseError) =>
          throw new IllegalArgumentException(s"Failed to parse argument path.\n${parser.formatError(e)}")
        case Failure(e) =>
          throw e
        case Success(x) =>
          new ArgumentPath(x)
      }
    }

    def apply(path: Uri.Path): ArgumentPath = {

      if (!path.absolute)
        throw new IllegalArgumentException("Expect absolute path.")

      new ArgumentPath(path)
    }
  }

  private class PathParser(val input: ParserInput)
    extends Parser
      with Uri.AbstractParser {

    def `path-params-expression`: Rule1[Seq[ParameterPath.Segment]] =
      rule {

        '/' ~ zeroOrMore(`segment-params`).separatedBy('/') ~ EOI
      }

    def `path-args-expression`: Rule1[Seq[String]] =
      rule {

        '/' ~ zeroOrMore(`segment`).separatedBy('/') ~ EOI ~>
          ((seq: Seq[String]) => seq.map(UriHelper.decode(_)))
      }

    def `segment-params`: Rule1[ParameterPath.Segment] =
      rule {

        '?' ~ capture(oneOrMore(SegmentChars)) ~ '<' ~ `regex` ~ '>' ~>
          ((x: String, r: Regex) => ParameterPath.Parameter(x, Some(r))) |
          '?' ~ capture(oneOrMore(SegmentChars)) ~>
            ((x: String) => ParameterPath.Parameter(x, None)) |
          capture(zeroOrMore(SegmentChars)) ~>
            ((x: String) => ParameterPath.Label(x))
      }

    def `regex`: Rule1[Regex] =
      rule {

        capture(oneOrMore(ANY ~ !('>' ~ '/' | '>' ~ EOI)) ~ ANY) ~>
          ((x: String) => x.r)
      }
  }

}

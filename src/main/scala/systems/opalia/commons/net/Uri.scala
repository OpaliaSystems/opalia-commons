package systems.opalia.commons.net

import java.nio.file.{Path => NioPath, Paths => NioPaths}
import org.parboiled2._
import scala.collection.immutable.LinearSeq
import scala.collection.{LinearSeqOptimized, mutable}
import scala.util.Success
import shapeless.HNil
import systems.opalia.interfaces.rendering._


sealed abstract case class Uri private(scheme: String,
                                       authority: Option[Uri.Authority],
                                       path: Uri.Path,
                                       queryStringRaw: Option[String],
                                       fragment: Option[String])
  extends StringRenderable {

  private val charsFragment = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@/?"

  def query(): Uri.Query =
    queryStringRaw.map(Uri.Query(_)).getOrElse(Uri.Query.Empty)

  def queryString: Option[String] =
    queryStringRaw.map(UriHelper.decode(_, replaceSpaces = true))

  def withScheme(scheme: String): Uri =
    copy(scheme = scheme)

  def withAuthority(authority: Uri.Authority): Uri =
    copy(authority = Some(authority))

  def withoutAuthority(): Uri =
    copy(authority = None)

  def withPath(path: Uri.Path): Uri =
    copy(path = path)

  def withoutPath(): Uri =
    copy(path = Uri.Path(Nil, path.pathType))

  def withQueryStringRaw(queryStringRaw: String): Uri =
    copy(queryStringRaw = Some(new Uri.UriParser(queryStringRaw).`query-expression`.run().get))

  def withQueryString(queryString: String): Uri =
    copy(queryStringRaw = Some(UriHelper.encode(
      queryString,
      UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@/?",
      replaceSpaces = true)))

  def withoutQueryString(): Uri =
    copy(queryStringRaw = None)

  def withQuery(query: Uri.Query): Uri =
    copy(queryStringRaw = if (query.isEmpty) None else Some(query.toString))

  def withFragment(fragment: String): Uri =
    copy(fragment = Some(fragment))

  def withoutFragment(): Uri =
    copy(fragment = None)

  def renderString(renderer: StringRenderer): StringRenderer = {

    def encFragment(string: String): String =
      UriHelper.encode(string, charsFragment)

    val _authority = authority.map(x => renderer.newEmpty ~ x).getOrElse(renderer.newEmpty)

    val _hierPart =
      path.pathType match {
        case Uri.Path.Type.Regular => renderer.newEmpty ~ "//" ~ _authority ~ path
        case _ => renderer.newEmpty ~ path
      }

    val q = queryStringRaw.map(x => renderer.newEmpty ~ '?' ~ x).getOrElse(renderer.newEmpty)
    val f = fragment.map(x => renderer.newEmpty ~ '#' ~ encFragment(x)).getOrElse(renderer.newEmpty)

    renderer ~ scheme ~ ':' ~ _hierPart ~ q ~ f
  }

  private def copy(scheme: String = scheme,
                   authority: Option[Uri.Authority] = authority,
                   path: Uri.Path = path,
                   queryStringRaw: Option[String] = queryStringRaw,
                   fragment: Option[String] = fragment): Uri =
    Uri.create(scheme, authority, path, queryStringRaw, fragment)
}

object Uri {

  def apply(uri: String): Uri =
    new UriParser(uri).`uri-expression`.run().get

  def apply(scheme: String,
            authority: Option[Uri.Authority] = None,
            path: Path,
            queryStringRaw: Option[String] = None,
            fragment: Option[String] = None): Uri =
    Uri.create(
      new UriParser(scheme).`scheme-expression`.run().get,
      authority,
      path,
      queryStringRaw.map(x => new UriParser(x).`query-expression`.run().get),
      fragment
    )

  private[net] def create(scheme: String,
                          authority: Option[Uri.Authority],
                          path: Path,
                          queryStringRaw: Option[String],
                          fragment: Option[String]): Uri = {

    if ((authority.nonEmpty && (path.pathType != Path.Type.Regular)) ||
      (path.nonEmpty && path.pathType == Path.Type.Undefined))
      throw new IllegalArgumentException("Incorrect path type.")

    if ((path.pathType == Path.Type.Rootless && !path.headOption.exists(_.nonEmpty)) ||
      (path.pathType == Path.Type.Minimal && path.headOption.exists(_.isEmpty)))
      throw new IllegalArgumentException("Expect path with non empty head segment.")

    new Uri(scheme, authority, path, queryStringRaw, fragment) {
    }
  }

  sealed abstract case class Authority private(host: Either[IpAddress, String],
                                               port: Option[BigInt],
                                               userInfo: Option[String])
    extends StringRenderable {

    private val charsHost = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims
    private val charsUserInfo = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":"

    def withHost(host: String): Authority =
      copy(host = new UriParser(host).`host-expression`.run().get)

    def withPort(port: BigInt): Authority =
      copy(port = Some(port))

    def withoutPort(): Authority =
      copy(port = None)

    def withUserInfo(userInfo: String): Authority =
      copy(userInfo = Some(userInfo))

    def withoutUserInfo(): Authority =
      copy(userInfo = None)

    override def equals(that: Any): Boolean =
      that match {

        case that: Authority if (
          this.host == that.host &&
            this.port == that.port &&
            this.userInfo == that.userInfo) => true
        case _ => false
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encHost(string: String): String =
        UriHelper.encode(string, charsHost)

      def encUserInfo(string: String): String =
        UriHelper.encode(string, charsUserInfo)

      val _host =
        host match {
          case Left(x) if (x.representation.style == IpAddress.Style.V4) => renderer.newEmpty ~ x
          case Left(x) => renderer.newEmpty ~ '[' ~ x ~ ']'
          case Right(x) => renderer.newEmpty ~ encHost(x)
        }

      val _port = port.map(x => renderer.newEmpty ~ ':' ~ x.toString).getOrElse(renderer.newEmpty)
      val _userInfo = userInfo.map(x => renderer.newEmpty ~ encUserInfo(x) ~ '@').getOrElse(renderer.newEmpty)

      renderer ~ _userInfo ~ _host ~ _port
    }

    private def copy(host: Either[IpAddress, String] = host,
                     port: Option[BigInt] = port,
                     userInfo: Option[String] = userInfo): Authority =
      Authority.create(host, port, userInfo)
  }

  object Authority {

    def apply(host: IpAddress, port: BigInt, userInfo: String): Authority =
      Authority.create(
        Left(host),
        Some(port),
        Some(userInfo)
      )

    def apply(host: IpAddress, port: BigInt): Authority =
      Authority.create(
        Left(host),
        Some(port),
        None
      )

    def apply(host: IpAddress, userInfo: String): Authority =
      Authority.create(
        Left(host),
        None,
        Some(userInfo)
      )

    def apply(host: IpAddress): Authority =
      Authority.create(
        Left(host),
        None,
        None
      )

    def apply(host: String, port: BigInt, userInfo: String): Authority =
      Authority.create(
        new UriParser(host).`host-expression`.run().get,
        Some(port),
        Some(userInfo)
      )

    def apply(host: String, port: BigInt): Authority =
      Authority.create(
        new UriParser(host).`host-expression`.run().get,
        Some(port),
        None
      )

    def apply(host: String, userInfo: String): Authority =
      Authority.create(
        new UriParser(host).`host-expression`.run().get,
        None,
        Some(userInfo)
      )

    def apply(host: String): Authority =
      Authority.create(
        new UriParser(host).`host-expression`.run().get,
        None,
        None
      )

    private[net] def create(host: Either[IpAddress, String],
                            port: Option[BigInt],
                            userInfo: Option[String]): Authority = {

      if (port.exists(_ < 0))
        throw new NumberFormatException("URI port cannot be negative.")

      new Authority(host, port, userInfo) {
      }
    }
  }

  sealed abstract class Path private(segments: Seq[String], val pathType: Path.Type)
    extends LinearSeq[String]
      with LinearSeqOptimized[String, Path]
      with StringRenderable {

    private val charsSegment = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@"

    val absolute: Boolean = pathType == Path.Type.Regular || pathType == Path.Type.Minimal
    val relative: Boolean = pathType == Path.Type.Rootless

    override def isEmpty: Boolean =
      segments.isEmpty

    override def head: String =
      segments.head

    override def tail: Path =
      Path.create(segments.tail, Path.Type.Rootless)

    def toNioPath: NioPath =
      NioPaths.get(this.toString)

    def /(path: Path): Path =
      if (path.absolute)
        Path.create(path, path.pathType)
      else
        Path.create(this ++ path, pathType)

    def /(segment: String): Path =
      Path.create(this :+ segment, pathType)

    override def equals(that: Any): Boolean =
      that match {

        case that: Path if (this.sameElements(that) && this.pathType == that.pathType) => true
        case _ => false
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encSegment(string: String): String =
        UriHelper.encode(string, charsSegment)

      val _head = if (absolute && segments.nonEmpty) renderer.newEmpty ~ '/' else renderer.newEmpty
      val _segments = renderer.newEmpty.glue(segments.map(x => renderer.newEmpty ~ encSegment(x)), "/")

      renderer ~ _head ~ _segments
    }

    override protected def newBuilder: mutable.Builder[String, Path] =
      new mutable.Builder[String, Path] {

        private val buffer = mutable.ArrayBuffer.newBuilder[String]

        def +=(elem: String): this.type = {

          buffer += elem
          this
        }

        def clear(): Unit =
          buffer.clear()

        def result(): Path =
          Path.create(buffer.result(), pathType)
      }
  }

  object Path {

    def apply(path: String, pathType: Path.Type): Path = {

      if (path.nonEmpty && pathType == Path.Type.Undefined)
        throw new IllegalArgumentException("Expect empty path.")

      val result =
        pathType match {
          case Path.Type.Regular => new UriParser(path).`path-abempty-expression`.run()
          case Path.Type.Rootless => new UriParser(path).`path-rootless-expression`.run()
          case Path.Type.Minimal => new UriParser(path).`path-absolute-expression`.run()
          case Path.Type.Undefined => Success(apply(Nil, Path.Type.Undefined))
        }

      result.get
    }

    def apply(segments: Seq[String],
              pathType: Path.Type): Path =
      Path.create(segments, pathType)

    def unapply(path: Path): Option[String] =
      Some(path.toString)

    def unapply(uri: Uri): Option[String] =
      unapply(uri.path)

    def empty(pathType: Path.Type = Path.Type.Regular): Path =
      Path.create(Nil, pathType)

    private[net] def create(segments: Seq[String],
                            pathType: Path.Type): Path = {

      new Path(segments, pathType) {
      }
    }

    sealed trait Type

    object Type {

      case object Regular
        extends Type

      case object Rootless
        extends Type

      case object Minimal
        extends Type

      case object Undefined
        extends Type

    }

  }

  sealed abstract class Query private(arguments: Seq[(String, String)])
    extends LinearSeq[(String, String)]
      with LinearSeqOptimized[(String, String), Query]
      with StringRenderable {

    private val charsArgument = UriHelper.Chars.unreserved + "!$'()*,;" + ":@/?"

    override def isEmpty: Boolean =
      arguments.isEmpty

    override def head: (String, String) =
      arguments.head

    override def tail: Query =
      Query.create(arguments.tail)

    def withArgument(key: String, value: String): Query =
      Query.create(this :+ (key -> value))

    def withArgumentOnce(key: String, value: String): Query =
      withoutArgument(key).withArgument(key, value)

    def withoutArgument(key: String): Query =
      this.filterNot(_._1 == key)

    def toMultiMap: Map[String, Seq[String]] =
      this.groupBy(x => x._1).mapValues(x => x.map(_._2))

    override def equals(that: Any): Boolean =
      that match {

        case that: Query if (this.sameElements(that)) => true
        case _ => false
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encQuery(string: String): String =
        UriHelper.encode(string, charsArgument, replaceSpaces = true)

      val _arguments = renderer.newEmpty.glue(arguments.map(x =>
        renderer.newEmpty ~ encQuery(x._1) ~ '=' ~ encQuery(x._2)), "&")

      renderer ~ _arguments
    }

    override protected def newBuilder: mutable.Builder[(String, String), Query] =
      new mutable.Builder[(String, String), Query] {

        private val buffer = mutable.ArrayBuffer.newBuilder[(String, String)]

        def +=(elem: (String, String)): this.type = {

          buffer += elem
          this
        }

        def clear(): Unit =
          buffer.clear()

        def result(): Query =
          Query.create(buffer.result())
      }
  }

  object Query {

    def apply(queryStringRaw: String): Query =
      new UriParser(queryStringRaw).`query-pairs-expression`.run().get

    def apply(arguments: Seq[(String, String)]): Query =
      Query.create(arguments)

    private[net] def create(arguments: Seq[(String, String)]): Query = {

      if (arguments.exists(_._1.isEmpty))
        throw new IllegalArgumentException("Expect non empty keys in query string.")

      new Query(arguments) {
      }
    }

    case object Empty
      extends Query(Nil)

  }

  private class UriParser(val input: ParserInput)
    extends Parser
      with AbstractParser {

    def `scheme-expression`: Rule1[String] =
      rule {

        `scheme` ~ EOI
      }

    def `host-expression`: Rule1[Either[IpAddress, String]] =
      rule {

        (`ipv6-adr` ~ EOI ~> ((x: IpAddress) => Left(x))) |
          (`ipv4-adr` ~ EOI ~> ((x: IpAddress) => Left(x))) |
          (capture(oneOrMore(ANY)) ~ EOI ~> ((x: String) => Right(x)))
      }

    def `path-abempty-expression`: Rule1[Path] =
      rule {

        `path-abempty` ~ EOI
      }

    def `path-absolute-expression`: Rule1[Path] =
      rule {

        `path-absolute` ~ EOI
      }

    def `path-rootless-expression`: Rule1[Path] =
      rule {

        `path-rootless` ~ EOI
      }
  }

  private[net] trait AbstractParser
    extends IpAddress.AbstractParser {
    self: Parser =>

    // http://tools.ietf.org/html/rfc3986#appendix-A
    // http://tools.ietf.org/html/rfc8089#appendix-F

    val `unreserved` = CharPredicate.AlphaNum ++ "-._~"
    val `gen-delims` = CharPredicate(UriHelper.Chars.gendelims)
    val `sub-delims` = CharPredicate(UriHelper.Chars.subdelims)
    val `reserved` = `gen-delims` ++ `sub-delims`

    val SchemeTailChars = CharPredicate.AlphaNum ++ "+-."
    val RegNameChars = `unreserved` ++ `sub-delims`
    val UserInfoChars = `unreserved` ++ `sub-delims` ++ ':'
    val SegmentChars = `unreserved` ++ `sub-delims` ++ ':' ++ '@'
    val QueryChars = `unreserved` ++ `sub-delims` ++ ':' ++ '@' ++ '/' ++ '?'
    val FragmentChars = `unreserved` ++ `sub-delims` ++ ':' ++ '@' ++ '/' ++ '?'

    def `uri-expression`: Rule1[Uri] =
      rule {

        `scheme` ~ ':' ~ `hier-part` ~ optional("?" ~ `query`) ~ optional("#" ~ `fragment`) ~ EOI ~> {
          (scheme: String,
           hierarchicalPart: (Option[Uri.Authority], Path),
           query: Option[String],
           fragment: Option[String]) =>

            Uri(scheme, hierarchicalPart._1, hierarchicalPart._2, query, fragment.map(UriHelper.decode(_)))
        }
      }

    def `hier-part`: Rule1[(Option[Uri.Authority], Path)] =
      rule {

        '/' ~ '/' ~ optional(`authority`) ~ `path-abempty` ~>
          ((authority: Option[Authority], path: Path) => (authority, path)) |
          `path-absolute` ~> ((path: Path) => (None, path)) |
          `path-rootless` ~> ((path: Path) => (None, path)) |
          MATCH ~ push((None, Path(Nil, Path.Type.Undefined)))
      }

    def `scheme`: Rule1[String] =
      rule {

        capture(CharPredicate.Alpha ~ zeroOrMore(SchemeTailChars))
      }

    def `authority`: Rule1[Uri.Authority] =
      rule {

        optional(`userinfo` ~ '@') ~ `host` ~ optional(':' ~ `port`) ~> {
          (userInfo: Option[String], host: Either[IpAddress, String], port: Option[BigInt]) =>

            Uri.Authority.create(host.map(UriHelper.decode(_)), port, userInfo.map(x => UriHelper.decode(x)))
        }
      }

    def `userinfo`: Rule1[String] =
      rule {

        capture(oneOrMore(UserInfoChars | `pct-encoded`))
      }

    def `host`: Rule1[Either[IpAddress, String]] =
      rule {

        (`ipv6-literal` ~ &(`gen-delims` | EOI) ~> ((x: IpAddress) => Left(x))) |
          (`ipv4-adr` ~ &(`gen-delims` | EOI) ~> ((x: IpAddress) => Left(x))) |
          (`reg-name` ~ &(`gen-delims` | EOI) ~> ((x: String) => Right(x)))
      }

    // In contrast to RFC 3986, no empty hostnames are allowed.
    def `reg-name`: Rule1[String] =
      rule {

        capture(oneOrMore(RegNameChars | `pct-encoded`))
      }

    def `path-abempty`: Rule1[Path] =
      rule {

        zeroOrMore('/' ~ `segment`) ~> {
          (list: Seq[String]) =>

            Path.create(list.map(UriHelper.decode(_)), Path.Type.Regular)
        }
      }

    def `path-absolute`: Rule1[Path] =
      rule {

        ('/' ~ `segment-nz` ~ zeroOrMore('/' ~ `segment`) ~>
          ((x: String, xs: Seq[String]) => x +: xs) | '/' ~ push(List(""))) ~> {
          (list: Seq[String]) =>

            Path.create(list.map(UriHelper.decode(_)), Path.Type.Minimal)
        }
      }

    def `path-rootless`: Rule1[Path] =
      rule {

        `segment-nz` ~ zeroOrMore('/' ~ `segment`) ~> ((x: String, xs: Seq[String]) => x +: xs) ~> {
          (list: Seq[String]) =>

            Path.create(list.map(UriHelper.decode(_)), Path.Type.Rootless)
        }
      }

    def `port`: Rule1[BigInt] =
      rule {

        capture((CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0') ~> ((x: String) => BigInt(x))
      }

    def `segment`: Rule1[String] =
      rule {

        capture(zeroOrMore(SegmentChars | `pct-encoded`))
      }

    def `segment-nz`: Rule1[String] =
      rule {

        capture(oneOrMore(SegmentChars | `pct-encoded`))
      }

    def `query`: Rule1[String] =
      rule {

        capture(zeroOrMore(QueryChars | `pct-encoded`))
      }

    def `query-expression`: Rule1[String] =
      rule {

        `query` ~ EOI
      }

    def `query-pairs-expression`: Rule1[Query] =
      rule {

        zeroOrMore(`query-pair`).separatedBy('&') ~ EOI ~> {
          (arguments: Seq[(String, String)]) =>

            Query.create(arguments)
        }
      }

    def `query-pair`: Rule1[(String, String)] =
      rule {

        capture(oneOrMore(!(anyOf("=&")) ~ (QueryChars | `pct-encoded`))) ~
          optional('=' ~ capture(zeroOrMore(!('&') ~ (QueryChars | `pct-encoded`)))) ~> {
          (key: String, argument: Option[String]) =>

            (UriHelper.decode(key, replaceSpaces = true),
              argument.map(x => UriHelper.decode(x, replaceSpaces = true)).getOrElse(""))
        }
      }

    def `fragment`: Rule1[String] =
      rule {

        capture(zeroOrMore(FragmentChars | `pct-encoded`))
      }

    def `pct-encoded`: Rule[HNil, HNil] =
      rule {

        '%' ~ CharPredicate.HexDigit ~ CharPredicate.HexDigit
      }
  }

}

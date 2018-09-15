package systems.opalia.commons.net

import java.nio.file.{Path => NioPath, Paths => NioPaths}
import scala.collection.immutable.LinearSeq
import scala.collection.{LinearSeqOptimized, mutable}
import systems.opalia.commons.utility.RegexParsersEx
import systems.opalia.interfaces.rendering._


sealed abstract case class Uri(scheme: String,
                               authority: Option[Uri.Authority],
                               path: Option[Uri.Path],
                               queryStringRaw: Option[String],
                               fragment: Option[String])
  extends StringRenderable {

  private val charsFragment = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@/?"

  def query(): Uri.Query =
    queryStringRaw.map(Uri.Query.create).getOrElse(Uri.Query.Empty)

  def queryString: Option[String] =
    queryStringRaw.map(UriHelper.decode(_, replaceSpaces = true))

  def withScheme(scheme: String): Uri =
    copy(scheme = scheme)

  def withAuthority(authority: Uri.Authority): Uri =
    copy(authority = Some(authority))

  def withoutAuthority(): Uri =
    copy(authority = None)

  def withPath(path: Uri.Path): Uri =
    copy(path = Some(path))

  def withoutPath(): Uri =
    copy(path = None)

  def withQueryStringRaw(queryStringRaw: String): Uri =
    copy(queryStringRaw = Some(Uri.Parser.parseQueryString(queryStringRaw)))

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

    val _authority = authority.map(x => renderer.newEmpty ~ "//" ~ x).getOrElse(renderer.newEmpty)

    val _path =
      if (authority.isEmpty && path.exists(_.absolute))
        path.map(x => renderer.newEmpty ~ "//" ~ x).getOrElse(renderer.newEmpty)
      else
        path.map(x => renderer.newEmpty ~ x).getOrElse(renderer.newEmpty)

    val q = queryStringRaw.map(x => renderer.newEmpty ~ '?' ~ x).getOrElse(renderer.newEmpty)
    val f = fragment.map(x => renderer.newEmpty ~ '#' ~ encFragment(x)).getOrElse(renderer.newEmpty)

    renderer ~ scheme ~ ':' ~ _authority ~ _path ~ q ~ f
  }

  private def copy(scheme: String = scheme,
                   authority: Option[Uri.Authority] = authority,
                   path: Option[Uri.Path] = path,
                   queryStringRaw: Option[String] = queryStringRaw,
                   fragment: Option[String] = fragment): Uri =
    Uri.create(scheme, authority, path, queryStringRaw, fragment)
}

object Uri {

  def apply(uri: String): Uri =
    Parser.parseUri(uri)

  def apply(scheme: String,
            authority: Option[Uri.Authority],
            path: Option[Path],
            queryStringRaw: Option[String],
            fragment: Option[String]): Uri = {

    val _scheme = Parser.parseScheme(scheme)
    val _queryStringRaw = queryStringRaw.map(Uri.Parser.parseQueryString)

    Uri.create(_scheme, authority, path, _queryStringRaw, fragment)
  }

  private[net] def create(scheme: String,
                          authority: Option[Uri.Authority],
                          path: Option[Path],
                          queryStringRaw: Option[String],
                          fragment: Option[String]): Uri = {

    if (authority.isDefined && path.exists(_.relative))
      throw new IllegalArgumentException("Expect absolute or empty path after authority.")

    if (authority.isEmpty && path.flatMap(_.headOption).exists(_.isEmpty))
      throw new IllegalArgumentException("Expect path with non empty head segment.")

    new Uri(scheme, authority, path, queryStringRaw, fragment) {
    }
  }

  private object Parser
    extends RegexParsersEx
      with IpAndPortParser {

    // http://tools.ietf.org/html/rfc3986#appendix-A
    // http://tools.ietf.org/html/rfc8089#appendix-F

    def `URI`: Parser[Uri] =
      (scheme <~ ":") ~ `hier-part` ~ ("?" ~> `query`).? ~ ("#" ~> `fragment`).? ^^ {
        case scheme ~ hierarchicalPart ~ query ~ fragment =>
          Uri(scheme, hierarchicalPart._1, hierarchicalPart._2, query, fragment.map(UriHelper.decode(_)))
      }

    def `hier-part`: Parser[(Option[Uri.Authority], Option[Path])] =
      ("//" ~> `authority` ~ `path-absolute-1`.?) ^^ { case authority ~ path => (Some(authority), path) } |
        (`path-absolute-3` | `path-absolute-2` | `path-relative`).? ^^ { x => (None, x) }

    def `scheme`: Parser[String] =
      """([a-zA-Z]([a-zA-Z0-9]|[+-.])*)""".r

    def `authority`: Parser[Uri.Authority] =
      (userinfo <~ "@").? ~ `host` ~ (":" ~> `PORT`).? ^^ {
        case userinfo ~ host ~ port =>
          Uri.Authority.create(UriHelper.decode(host._1), port, userinfo.map(x => UriHelper.decode(x)), host._2)
      }

    def `userinfo`: Parser[String] =
      """(([a-zA-Z0-9]|%[a-fA-F0-9]{2}|[-._~]|[!$&'()*+,;=]|[:])+)""".r

    // In contrast to RFC 3986, no empty hostnames are allowed.
    def `host`: Parser[(String, HostType.Value)] =
      `reg-name` ^^ ((_, HostType.Hostname)) |||
        `IPv4-ADR` ^^ ((_, HostType.IPv4)) |||
        `IPv6-LITERAL` ^^ ((_, HostType.IPv6))

    def `reg-name`: Parser[String] =
      """(([a-zA-Z0-9]|%[a-fA-F0-9]{2}|[-._~]|[!$&'()*+,;=])+)""".r

    def `path-absolute-1`: Parser[Path] =
      rep1("/" ~> `segment`.?) ^^ {
        list =>

          Path.create(list.map(_.map(UriHelper.decode(_)).getOrElse("")), absolute = true)
      }

    // In contrast to RFC 3986, an absolute path can start with /// to handle file URIs.
    def `path-absolute-2`: Parser[Path] =
      "///" ~> `segment` ~ rep("/" ~> `segment`.?) ^^ {
        case segment ~ rest =>

          val list = Some(segment) +: rest

          Path.create(list.map(_.map(UriHelper.decode(_)).getOrElse("")), absolute = true)
      }

    def `path-absolute-3`: Parser[Path] =
      "/" ~> `segment` ~ rep("/" ~> `segment`.?) ^^ {
        case segment ~ rest =>

          val list = Some(segment) +: rest

          Path.create(list.map(_.map(UriHelper.decode(_)).getOrElse("")), absolute = true)
      }

    def `path-relative`: Parser[Path] =
      segment ~ rep("/" ~> `segment`.?) ^^ {
        case segment ~ rest =>

          val list = Some(segment) +: rest

          Path.create(list.map(_.map(UriHelper.decode(_)).getOrElse("")), absolute = false)
      }

    def `segment`: Parser[String] =
      """(([a-zA-Z0-9]|%[a-fA-F0-9]{2}|[-._~]|[!$&'()*+,;=]|[:@])+)""".r

    def `query`: Parser[String] =
      """(([a-zA-Z0-9]|%[a-fA-F0-9]{2}|[-._~]|[!$&'()*+,;=]|[:@/?])*)""".r

    def `fragment`: Parser[String] =
      """(([a-zA-Z0-9]|%[a-fA-F0-9]{2}|[-._~]|[!$&'()*+,;=]|[:@/?])*)""".r

    def parseUri(value: String): Uri =
      parseAll(`URI`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }

    def parseScheme(value: String): String =
      parseAll(`scheme`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }

    def parseAuthority(value: String): Authority =
      parseAll(`authority`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }

    def parseHost(value: String): (String, HostType.Value) = {

      def expression: Parser[(String, HostType.Value)] =
        """.+""".r ^^ ((_, HostType.Hostname)) |||
          `IPv4-ADR` ^^ ((_, HostType.IPv4)) |||
          `IPv6-LITERAL` ^^ ((_, HostType.IPv6))

      parseAll(expression, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
    }

    def parsePort(value: String): Int =
      parseAll(`PORT`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }

    def parseUserInfo(value: String): String =
      parseAll(`userinfo`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }

    def parsePath(value: String): Path = {

      def expression: Parser[Path] =
        `path-absolute-1` | `path-relative`

      parseAll(expression, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
    }

    def parseQueryString(value: String): String =
      parseAll(`query`, value) match {
        case Success(result, _) => result
        case failure: NoSuccess => throw new IllegalArgumentException(failure.msg)
      }
  }

  sealed abstract case class Authority(host: String,
                                       port: Option[Int],
                                       userInfo: Option[String])
    extends StringRenderable {

    private val charsHost = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims
    private val charsUserInfo = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":"

    val hostType: HostType.Value

    def withHost(host: String): Authority = {

      val _host = Parser.parseHost(host)

      copy(host = _host._1, hostType = _host._2)
    }

    def withPort(port: Int): Authority =
      copy(port = Some(Parser.parsePort(port.toString)))

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
            this.userInfo == that.userInfo &&
            this.hostType == that.hostType) => true
        case _ => false
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encHost(string: String): String =
        UriHelper.encode(string, charsHost)

      def encUserInfo(string: String): String =
        UriHelper.encode(string, charsUserInfo)

      val _host =
        if (hostType == HostType.Hostname)
          renderer.newEmpty ~ encHost(host)
        else if (hostType == HostType.IPv6)
          renderer.newEmpty ~ '[' ~ host ~ ']'
        else
          renderer.newEmpty ~ host

      val _port = port.map(x => renderer.newEmpty ~ ':' ~ x).getOrElse(renderer.newEmpty)
      val _userInfo = userInfo.map(x => renderer.newEmpty ~ encUserInfo(x) ~ '@').getOrElse(renderer.newEmpty)

      renderer ~ _userInfo ~ _host ~ _port
    }

    private def copy(host: String = host,
                     port: Option[Int] = port,
                     userInfo: Option[String] = userInfo,
                     hostType: HostType.Value = hostType): Authority =
      Authority.create(host, port, userInfo, hostType)
  }

  object Authority {

    def apply(authority: String): Authority =
      Parser.parseAuthority(authority)

    def apply(host: String,
              port: Option[Int],
              userInfo: Option[String]): Authority = {

      val _host = Parser.parseHost(host)
      val _port = port.map(x => Parser.parsePort(x.toString))
      val _userInfo = userInfo

      Authority.create(_host._1, _port, _userInfo, _host._2)
    }

    private[net] def create(host: String,
                            port: Option[Int],
                            userInfo: Option[String],
                            hostType: HostType.Value): Authority = {

      val _hostType = hostType

      new Authority(host, port, userInfo) {

        val hostType = _hostType
      }
    }
  }

  sealed abstract class Path(segments: Seq[String], val absolute: Boolean)
    extends LinearSeq[String]
      with LinearSeqOptimized[String, Path]
      with StringRenderable {

    private val charsSegment = UriHelper.Chars.unreserved + UriHelper.Chars.subdelims + ":@"

    val relative: Boolean = !absolute

    override def isEmpty: Boolean =
      segments.isEmpty

    override def head: String =
      segments.head

    override def tail: Path =
      Path.create(segments.tail, absolute = false)

    def toAbsolutePath: Path =
      Path.create(segments, absolute = true)

    def toRelativePath: Path =
      Path.create(segments, absolute = false)

    def toNioPath: NioPath =
      NioPaths.get(this.toString)

    def /(path: Path): Path =
      if (path.absolute)
        Path.create(path, absolute = true)
      else
        Path.create(this ++ path, this.absolute)

    def /(segment: String): Path =
      Path.create(this :+ segment, this.absolute)

    override def equals(that: Any): Boolean =
      that match {

        case that: Path if (this.sameElements(that) && this.absolute == that.absolute) => true
        case _ => false
      }

    def renderString(renderer: StringRenderer): StringRenderer = {

      def encSegment(string: String): String =
        UriHelper.encode(string, charsSegment)

      val _head = if (absolute) renderer.newEmpty ~ '/' else renderer.newEmpty
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
          Path.create(buffer.result(), absolute)
      }
  }

  object Path {

    def apply(path: String): Path =
      Parser.parsePath(path)

    def apply(segments: Seq[String],
              absolute: Boolean): Path =
      Path.create(segments, absolute)

    def unapply(path: Path): Option[String] =
      Some(path.toString)

    def unapply(uri: Uri): Option[String] =
      uri.path.flatMap(unapply)

    def root: Path =
      Path.create(Nil, absolute = true)

    private[net] def create(segments: Seq[String],
                            absolute: Boolean): Path = {

      new Path(segments, absolute) {
      }
    }
  }

  sealed abstract class Query(arguments: Seq[(String, String)])
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

      def enc(string: String): String =
        UriHelper.encode(string, charsArgument, replaceSpaces = true)

      val _arguments = renderer.newEmpty.glue(arguments.map(x => renderer.newEmpty ~ enc(x._1) ~ '=' ~ enc(x._2)), "&")

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
      Query.create(Uri.Parser.parseQueryString(queryStringRaw))

    def apply(arguments: Seq[(String, String)]): Query =
      Query.create(arguments)

    private[net] def create(queryStringRaw: String): Query = {

      val arguments =
        queryStringRaw.split('&').toSeq
          .filter(_.nonEmpty)
          .map {
            argument =>

              val index = argument.indexOf('=')

              if (index <= 0)
                (argument, "")
              else
                (argument.take(index), argument.drop(index + 1))
          }
          .map(x => (UriHelper.decode(x._1, replaceSpaces = true), UriHelper.decode(x._2, replaceSpaces = true)))

      Query.create(arguments)
    }

    private[net] def create(arguments: Seq[(String, String)]): Query = {

      if (arguments.exists(_._1.isEmpty))
        throw new IllegalArgumentException("Expect non empty keys in query string.")

      new Query(arguments) {
      }
    }

    case object Empty
      extends Query(Nil)

  }

}

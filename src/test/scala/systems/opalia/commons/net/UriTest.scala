package systems.opalia.commons.net

import org.parboiled2.ParseError
import org.scalatest._


class UriTest
  extends FlatSpec
    with Matchers {

  // Here are some parts of code copied from akka HTTP project to get good test cases.
  // https://github.com/akka/akka-http/blob/master/akka-http-core/src/test/scala/akka/http/scaladsl/model/UriSpec.scala

  object Examples {

    val _01 = "ftp://ftp.is.co.za/rfc/rfc1808.txt"
    val _02 = "https://en.wikipedia.org:80/wiki/Uniform_Resource_Identifier#Syntax"
    val _03 = "ldap://[2001:db8::7]/c=GB?objectClass?one"
    val _04 = "mailto:John.Doe@example.com"
    val _05 = "file:///C:/Users/Desktop/index.html"
    val _06 = "file:///etc/fstab"
    val _07 = "file://localhost/etc/fstab"
    val _08 = "jar:../lib/META-INF/manifest.mf"
    val _09 = "zip:http://downloads/somefile.zip"
    val _10 = "jar:zip:outer.zip!/nested.jar!/somedir"
    val _11 = "jar:zip:outer.zip!/nested.jar!/%C3%A4%C3%B6%C3%BC"
    val _12 = "tar.gz:./path/in/tar/README.txt"
    val _13 = "geo:48.33,14.122;u=22.5"
    val _14 = "news:comp.infosystems.www.servers.unix"
    val _15 = "tel:+1-816-555-1212"
    val _16 = "telnet://192.0.2.16:80/"
    val _17 = "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    val _18 = "urn:uuid:6e8bc430-9c3a-11d9-9669-0800200c9a42"
    val _19 = "urn:isbn:4273?author=unknown"
    val _20 = "crid://broadcaster.tld/best/movies"
    val _21 = "http://user:password@example.org:8080/cgi-bin/script.php?action=submit&pageid=86392001#section_2"
    val _22 = "http://de.wikipedia.org"
    val _23 = "http://de.wikipedia.org/"
    val _24 = "http://192.0.2.16:80"
    val _25 = "smb://host/home"
    val _26 = "res:path/in/classpath/image.png"
    val _27 = "res://host?"
    val _28 = "res://host?#"
    val _29 = "res:?"
    val _30 = "res:?#"
    val _31 = "file:/etc/fstab"
  }

  it should "be able to parse simple test examples" in {

    val _01 = Uri(Examples._01)

    _01.scheme shouldBe "ftp"
    _01.authority shouldBe Some(Uri.Authority.create(Right("ftp.is.co.za"), None, None))
    _01.path shouldBe Uri.Path.create(List("rfc", "rfc1808.txt"), Uri.Path.Type.Regular)
    _01.queryStringRaw shouldBe None
    _01.fragment shouldBe None

    val _02 = Uri(Examples._02)

    _02.scheme shouldBe "https"
    _02.authority shouldBe Some(Uri.Authority.create(Right("en.wikipedia.org"), Some(80), None))
    _02.path shouldBe Uri.Path.create(List("wiki", "Uniform_Resource_Identifier"), Uri.Path.Type.Regular)
    _02.queryStringRaw shouldBe None
    _02.fragment shouldBe Some("Syntax")

    val _03 = Uri(Examples._03)

    _03.scheme shouldBe "ldap"
    _03.authority shouldBe Some(Uri.Authority.create(Left(IpAddress("2001:db8::7")), None, None))
    _03.path shouldBe Uri.Path.create(List("c=GB"), Uri.Path.Type.Regular)
    _03.queryStringRaw shouldBe Some("objectClass?one")
    _03.fragment shouldBe None

    val _04 = Uri(Examples._04)

    _04.scheme shouldBe "mailto"
    _04.authority shouldBe None
    _04.path shouldBe Uri.Path.create(List("John.Doe@example.com"), Uri.Path.Type.Rootless)
    _04.queryStringRaw shouldBe None
    _04.fragment shouldBe None

    val _05 = Uri(Examples._05)

    _05.scheme shouldBe "file"
    _05.authority shouldBe None
    _05.path shouldBe Uri.Path.create(List("C:", "Users", "Desktop", "index.html"), Uri.Path.Type.Regular)
    _05.queryStringRaw shouldBe None
    _05.fragment shouldBe None

    val _06 = Uri(Examples._06)

    _06.scheme shouldBe "file"
    _06.authority shouldBe None
    _06.path shouldBe Uri.Path.create(List("etc", "fstab"), Uri.Path.Type.Regular)
    _06.queryStringRaw shouldBe None
    _06.fragment shouldBe None

    val _07 = Uri(Examples._07)

    _07.scheme shouldBe "file"
    _07.authority shouldBe Some(Uri.Authority.create(Right("localhost"), None, None))
    _07.path shouldBe Uri.Path.create(List("etc", "fstab"), Uri.Path.Type.Regular)
    _07.queryStringRaw shouldBe None
    _07.fragment shouldBe None

    val _08 = Uri(Examples._08)

    _08.scheme shouldBe "jar"
    _08.authority shouldBe None
    _08.path shouldBe Uri.Path.create(List("..", "lib", "META-INF", "manifest.mf"), Uri.Path.Type.Rootless)
    _08.queryStringRaw shouldBe None
    _08.fragment shouldBe None

    val _09 = Uri(Examples._09)

    _09.scheme shouldBe "zip"
    _09.authority shouldBe None
    _09.path shouldBe Uri.Path.create(List("http:", "", "downloads", "somefile.zip"), Uri.Path.Type.Rootless)
    _09.queryStringRaw shouldBe None
    _09.fragment shouldBe None

    val _10 = Uri(Examples._10)

    _10.scheme shouldBe "jar"
    _10.authority shouldBe None
    _10.path shouldBe Uri.Path.create(List("zip:outer.zip!", "nested.jar!", "somedir"), Uri.Path.Type.Rootless)
    _10.queryStringRaw shouldBe None
    _10.fragment shouldBe None

    val _11 = Uri(Examples._11)

    _11.scheme shouldBe "jar"
    _11.authority shouldBe None
    _11.path shouldBe Uri.Path.create(List("zip:outer.zip!", "nested.jar!", "äöü"), Uri.Path.Type.Rootless)
    _11.queryStringRaw shouldBe None
    _11.fragment shouldBe None

    val _12 = Uri(Examples._12)

    _12.scheme shouldBe "tar.gz"
    _12.authority shouldBe None
    _12.path shouldBe Uri.Path.create(List(".", "path", "in", "tar", "README.txt"), Uri.Path.Type.Rootless)
    _12.queryStringRaw shouldBe None
    _12.fragment shouldBe None

    val _13 = Uri(Examples._13)

    _13.scheme shouldBe "geo"
    _13.authority shouldBe None
    _13.path shouldBe Uri.Path.create(List("48.33,14.122;u=22.5"), Uri.Path.Type.Rootless)
    _13.queryStringRaw shouldBe None
    _13.fragment shouldBe None

    val _14 = Uri(Examples._14)

    _14.scheme shouldBe "news"
    _14.authority shouldBe None
    _14.path shouldBe Uri.Path.create(List("comp.infosystems.www.servers.unix"), Uri.Path.Type.Rootless)
    _14.queryStringRaw shouldBe None
    _14.fragment shouldBe None

    val _15 = Uri(Examples._15)

    _15.scheme shouldBe "tel"
    _15.authority shouldBe None
    _15.path shouldBe Uri.Path.create(List("+1-816-555-1212"), Uri.Path.Type.Rootless)
    _15.queryStringRaw shouldBe None
    _15.fragment shouldBe None

    val _16 = Uri(Examples._16)

    _16.scheme shouldBe "telnet"
    _16.authority shouldBe Some(Uri.Authority.create(Left(IpAddress("192.0.2.16")), Some(80), None))
    _16.path shouldBe Uri.Path.create(List(""), Uri.Path.Type.Regular)
    _16.queryStringRaw shouldBe None
    _16.fragment shouldBe None

    val _17 = Uri(Examples._17)

    _17.scheme shouldBe "urn"
    _17.authority shouldBe None
    _17.path shouldBe Uri.Path.create(List("oasis:names:specification:docbook:dtd:xml:4.1.2"), Uri.Path.Type.Rootless)
    _17.queryStringRaw shouldBe None
    _17.fragment shouldBe None

    val _18 = Uri(Examples._18)

    _18.scheme shouldBe "urn"
    _18.authority shouldBe None
    _18.path shouldBe Uri.Path.create(List("uuid:6e8bc430-9c3a-11d9-9669-0800200c9a42"), Uri.Path.Type.Rootless)
    _18.queryStringRaw shouldBe None
    _18.fragment shouldBe None

    val _19 = Uri(Examples._19)

    _19.scheme shouldBe "urn"
    _19.authority shouldBe None
    _19.path shouldBe Uri.Path.create(List("isbn:4273"), Uri.Path.Type.Rootless)
    _19.queryStringRaw shouldBe Some("author=unknown")
    _19.fragment shouldBe None

    val _20 = Uri(Examples._20)

    _20.scheme shouldBe "crid"
    _20.authority shouldBe Some(Uri.Authority.create(Right("broadcaster.tld"), None, None))
    _20.path shouldBe Uri.Path.create(List("best", "movies"), Uri.Path.Type.Regular)
    _20.queryStringRaw shouldBe None
    _20.fragment shouldBe None

    val _21 = Uri(Examples._21)

    _21.scheme shouldBe "http"
    _21.authority shouldBe Some(Uri.Authority.create(Right("example.org"), Some(8080), Some("user:password")))
    _21.path shouldBe Uri.Path.create(List("cgi-bin", "script.php"), Uri.Path.Type.Regular)
    _21.queryStringRaw shouldBe Some("action=submit&pageid=86392001")
    _21.fragment shouldBe Some("section_2")

    val _22 = Uri(Examples._22)

    _22.scheme shouldBe "http"
    _22.authority shouldBe Some(Uri.Authority.create(Right("de.wikipedia.org"), None, None))
    _22.path shouldBe Uri.Path.empty(Uri.Path.Type.Regular)
    _22.queryStringRaw shouldBe None
    _22.fragment shouldBe None

    val _23 = Uri(Examples._23)

    _23.scheme shouldBe "http"
    _23.authority shouldBe Some(Uri.Authority.create(Right("de.wikipedia.org"), None, None))
    _23.path shouldBe Uri.Path.create(List(""), Uri.Path.Type.Regular)
    _23.queryStringRaw shouldBe None
    _23.fragment shouldBe None

    val _24 = Uri(Examples._24)

    _24.scheme shouldBe "http"
    _24.authority shouldBe Some(Uri.Authority.create(Left(IpAddress("192.0.2.16")), Some(80), None))
    _24.path shouldBe Uri.Path.empty(Uri.Path.Type.Regular)
    _24.queryStringRaw shouldBe None
    _24.fragment shouldBe None

    val _25 = Uri(Examples._25)

    _25.scheme shouldBe "smb"
    _25.authority shouldBe Some(Uri.Authority.create(Right("host"), None, None))
    _25.path shouldBe Uri.Path.create(List("home"), Uri.Path.Type.Regular)
    _25.queryStringRaw shouldBe None
    _25.fragment shouldBe None

    val _26 = Uri(Examples._26)

    _26.scheme shouldBe "res"
    _26.authority shouldBe None
    _26.path shouldBe Uri.Path.create(List("path", "in", "classpath", "image.png"), Uri.Path.Type.Rootless)
    _26.queryStringRaw shouldBe None
    _26.fragment shouldBe None

    val _27 = Uri(Examples._27)

    _27.scheme shouldBe "res"
    _27.authority shouldBe Some(Uri.Authority.create(Right("host"), None, None))
    _27.path shouldBe Uri.Path.empty(Uri.Path.Type.Regular)
    _27.queryStringRaw shouldBe Some("")
    _27.fragment shouldBe None

    val _28 = Uri(Examples._28)

    _28.scheme shouldBe "res"
    _28.authority shouldBe Some(Uri.Authority.create(Right("host"), None, None))
    _28.path shouldBe Uri.Path.empty(Uri.Path.Type.Regular)
    _28.queryStringRaw shouldBe Some("")
    _28.fragment shouldBe Some("")

    val _29 = Uri(Examples._29)

    _29.scheme shouldBe "res"
    _29.authority shouldBe None
    _29.path shouldBe Uri.Path.empty(Uri.Path.Type.Undefined)
    _29.queryStringRaw shouldBe Some("")
    _29.fragment shouldBe None

    val _30 = Uri(Examples._30)

    _30.scheme shouldBe "res"
    _30.authority shouldBe None
    _30.path shouldBe Uri.Path.empty(Uri.Path.Type.Undefined)
    _30.queryStringRaw shouldBe Some("")
    _30.fragment shouldBe Some("")

    val _31 = Uri(Examples._31)

    _31.scheme shouldBe "file"
    _31.authority shouldBe None
    _31.path shouldBe Uri.Path.create(List("etc", "fstab"), Uri.Path.Type.Minimal)
    _31.queryStringRaw shouldBe None
    _31.fragment shouldBe None
  }

  it should "be able to render simple test examples" in {

    Uri(Examples._01).toString shouldBe Examples._01
    Uri(Examples._02).toString shouldBe Examples._02
    Uri(Examples._03).toString shouldBe Examples._03
    Uri(Examples._04).toString shouldBe Examples._04
    Uri(Examples._05).toString shouldBe Examples._05
    Uri(Examples._06).toString shouldBe Examples._06
    Uri(Examples._07).toString shouldBe Examples._07
    Uri(Examples._08).toString shouldBe Examples._08
    Uri(Examples._09).toString shouldBe Examples._09
    Uri(Examples._10).toString shouldBe Examples._10
    Uri(Examples._11).toString shouldBe Examples._11
    Uri(Examples._12).toString shouldBe Examples._12
    Uri(Examples._13).toString shouldBe Examples._13
    Uri(Examples._14).toString shouldBe Examples._14
    Uri(Examples._15).toString shouldBe Examples._15
    Uri(Examples._16).toString shouldBe Examples._16
    Uri(Examples._17).toString shouldBe Examples._17
    Uri(Examples._18).toString shouldBe Examples._18
    Uri(Examples._19).toString shouldBe Examples._19
    Uri(Examples._20).toString shouldBe Examples._20
    Uri(Examples._21).toString shouldBe Examples._21
    Uri(Examples._22).toString shouldBe Examples._22
    Uri(Examples._23).toString shouldBe Examples._23
    Uri(Examples._24).toString shouldBe Examples._24
    Uri(Examples._25).toString shouldBe Examples._25
    Uri(Examples._26).toString shouldBe Examples._26
    Uri(Examples._27).toString shouldBe Examples._27
    Uri(Examples._28).toString shouldBe Examples._28
    Uri(Examples._29).toString shouldBe Examples._29
    Uri(Examples._30).toString shouldBe Examples._30
    Uri(Examples._31).toString shouldBe Examples._31
  }

  it should "produce proper error messages for illegal inputs" in {

    //illegal scheme
    an[ParseError] should be thrownBy Uri("foö:/a")

    // illegal user info
    an[ParseError] should be thrownBy Uri("http://user:ö@host")

    // illegal percent encoding
    an[ParseError] should be thrownBy Uri("http://use%2G@host")

    // illegal path
    an[ParseError] should be thrownBy Uri("http://www.example.com/name with spaces/")

    // illegal path with control character
    an[ParseError] should be thrownBy Uri("http:///with\newline")
  }

  it should "accept illegal IPv4 literals as hostname" in {

    Uri("http://01.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://001.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://00.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://000.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://256.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://300.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://1111.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://-1.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://0.0.0.").authority.get.host.isRight shouldBe true
    Uri("http://0.0.0.0.").authority.get.host.isRight shouldBe true
    Uri("http://0.0.0.0.0").authority.get.host.isRight shouldBe true
    Uri("http://0.0..0").authority.get.host.isRight shouldBe true
    Uri("http://.0.0.0").authority.get.host.isRight shouldBe true
  }

  it should "provide sugar for fluent transformations" in {

    val uri = Uri("http://user:pw@host:80/path?query#fragment")

    uri.withScheme("https").toString shouldBe
      "https://user:pw@host:80/path?query#fragment"

    uri.authority.map(x => uri.withAuthority(x.withHost("localhost")).toString) shouldBe
      Some("http://user:pw@localhost:80/path?query#fragment")

    uri.authority.map(x => uri.withAuthority(x.withPort(42)).toString) shouldBe
      Some("http://user:pw@host:42/path?query#fragment")

    uri.authority.map(x => uri.withAuthority(x.withoutPort()).toString) shouldBe
      Some("http://user:pw@host/path?query#fragment")

    uri.authority.map(x => uri.withAuthority(x.withUserInfo("John.Doe:123456")).toString) shouldBe
      Some("http://John.Doe:123456@host:80/path?query#fragment")

    uri.authority.map(x => uri.withAuthority(x.withoutUserInfo()).toString) shouldBe
      Some("http://host:80/path?query#fragment")

    // build uri with incorrect URI type throws exception
    an[IllegalArgumentException] should be thrownBy uri.withPath(Uri.Path(Nil, Uri.Path.Type.Rootless))
    an[IllegalArgumentException] should be thrownBy uri.withPath(Uri.Path(Nil, Uri.Path.Type.Minimal))

    // absolute path without authority and path with empty head throws exception
    an[IllegalArgumentException] should be thrownBy uri.withoutAuthority()
      .withPath(Uri.Path(List(""), Uri.Path.Type.Rootless))

    uri.withPath(Uri.Path("/path/to/resource", Uri.Path.Type.Regular)).toString shouldBe
      "http://user:pw@host:80/path/to/resource?query#fragment"

    uri.withPath(Uri.Path.empty() / "path" / "to" / "resource").toString shouldBe
      "http://user:pw@host:80/path/to/resource?query#fragment"

    uri.withoutPath().toString shouldBe
      "http://user:pw@host:80?query#fragment"

    uri.withQueryStringRaw("%C3%A4=1&a=2").toString shouldBe
      "http://user:pw@host:80/path?%C3%A4=1&a=2#fragment"

    uri.withQueryString("ä=1&a=2").toString shouldBe
      "http://user:pw@host:80/path?%C3%A4=1&a=2#fragment"

    uri.withoutQueryString().toString shouldBe
      "http://user:pw@host:80/path#fragment"

    uri.withQuery(uri.query()
      .withoutArgument("query")
      .withArgumentOnce("a", "foo")
      .withArgumentOnce("a", "bar")
      .withArgumentOnce("a", "baz")
      .withArgument("b", "foo")
      .withArgument("b", "bar")
      .withArgument("b", "baz")).toString shouldBe
      "http://user:pw@host:80/path?a=baz&b=foo&b=bar&b=baz#fragment"

    // empty keys in query string throws exception
    an[IllegalArgumentException] should be thrownBy uri.withQuery(Uri.Query.Empty.withArgument("", "value"))

    uri.withFragment("section_2").toString shouldBe
      "http://user:pw@host:80/path?query#section_2"

    uri.withoutFragment().toString shouldBe
      "http://user:pw@host:80/path?query"
  }

  it should "handle query arguments correctly" in {

    val query1 = Uri.Query("%C3%A4=%C3%B6%C3%BC&a=foo&b=bar&c=baz")
    val query2 = Uri.Query("a=1st+argument&a=2nd+argument&a=3rd+argument&b=foo")

    query1.toString shouldBe "%C3%A4=%C3%B6%C3%BC&a=foo&b=bar&c=baz"
    query2.toString shouldBe "a=1st+argument&a=2nd+argument&a=3rd+argument&b=foo"

    query1.toMap shouldBe Map("ä" -> "öü", "a" -> "foo", "b" -> "bar", "c" -> "baz")
    query2.toMultiMap shouldBe Map("a" -> List("1st argument", "2nd argument", "3rd argument"), "b" -> List("foo"))
  }
}

package systems.opalia.commons.net

import java.net.URLEncoder
import org.scalatest._


class UriHelperTest
  extends FlatSpec
    with Matchers {

  it should "encoded and decoded a URL correctly" in {

    val in = """1234 567890/\'!"§$%&*=?éä+-_#,.;:|@µabc()[]{}<>~"""
    val keepChars = UriHelper.Chars.alphanumeric + "*-._"

    val encoded = UriHelper.encode(in, keepChars, replaceSpaces = true)

    encoded should be(URLEncoder.encode(in, "UTF-8"))

    val decoded = UriHelper.decode(encoded, replaceSpaces = true)

    decoded should be(in)

  }
}
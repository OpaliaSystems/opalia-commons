package systems.opalia.commons.net

import org.parboiled2.ParseError
import org.scalatest._


class IpAddressTest
  extends FlatSpec
    with Matchers {

  it should "parse a valid list of IPv4 addresses" in {

    val listIn = List(
      "0.0.0.0", "255.255.255.255", "127.0.0.1", "10.0.0.1", "192.168.1.1"
    )

    val listOut = listIn.map(IpAddress(_))

    listIn.zip(listOut).foreach {
      case (origin, address) =>

        address.toString shouldBe origin
        address.representation.style shouldBe IpAddress.Style.V4
    }
  }

  it should "parse a valid list of IPv6 addresses with IPv4 part" in {

    val list1 =
      List(
        "0:0:0:0:0:0:0.0.0.0", "0000:0000:0000:0000:0000:0000:0.0.0.0", "1:2:3:4:5:6:127.0.0.1",
        "1:2:3:4:5::127.0.0.1", "1:2:3::6:127.0.0.1", "::6:255.255.255.255", "::2:3:4:5:6:255.255.255.255",
        "::255.255.255.255", "::ffff:10.0.0.1"
      )

    val list2 =
      (1 to 4)
        .map(x => (1 to x, 1 to (5 - x)))
        .flatMap(x => for (a <- x._1; b <- x._2) yield ("1:" * a) + ":" + ("1:" * b) + "0.0.0.0")
        .distinct

    val list3 =
      for (x <- 1 to 5) yield ("1:" * x) + ":" + "0.0.0.0"

    val list4 =
      for (x <- 0 to 5) yield "::" + ("1:" * x) + "0.0.0.0"

    val listIn =
      list1 ++ list2 ++ list3 ++ list4

    val listOut = listIn.map(IpAddress(_))

    listIn.zip(listOut).foreach {
      case (origin, address) =>

        address.toString shouldBe origin
        address.representation.style shouldBe IpAddress.Style.V4InV6
    }
  }

  it should "parse a valid list of IPv6 addresses" in {

    val list1 =
      List(
        "::", "0:0:0:0:0:0:0:0", "0000:0000:0000:0000:0000:0000:0000:0000", "2001:0db8:85a3:08d3:1319:8a2e:0370:0073",
        "2001:db8:85a3:8d3:1319:8a2e:370:73", "1:2:3:4:5:6:7:8", "::2:3:4:5:6:7:8", "1::3:4:5:6:7:8", "1:2::4:5:6:7:8",
        "1:2:3::5:6:7:8", "1:2:3:4::6:7:8", "1:2:3:4:5::7:8", "1:2:3:4:5:6::8", "1:2:3:4:5:6:7::", "1:2:3:4:5:6::",
        "1:2:3:4:5::", "1:2:3:4::", "1:2:3::", "1:2::", "1::", "::8", "1::8", "1:2::7:8", "fe08::7:8"
      )

    val list2 =
      (1 to 6)
        .map(x => (1 to x, 1 to (7 - x)))
        .flatMap(x => for (a <- x._1; b <- x._2) yield ("1:" * a) + (":1" * b))
        .distinct

    val list3 =
      for (x <- 1 to 7) yield ("1:" * x) + ":"

    val list4 =
      for (x <- 1 to 7) yield ":" + (":1" * x)

    val listIn =
      list1 ++ list2 ++ list3 ++ list4

    val listOut = listIn.map(IpAddress(_))

    listIn.zip(listOut).foreach {
      case (origin, address) =>

        address.toString shouldBe origin
        address.representation.style shouldBe IpAddress.Style.V6
    }
  }

  it should "throw an exception while parsing an invalid list of IP addresses" in {

    val listIn = List(
      ":::", ":::1234", "1234:::1234:1234", "1234:1234:::1234", "1234:::", "::12345", "abcd::abcd::abcd",
      "2001::eab:dead::a0:abcd:4e", "20011:0db8:85a3:08d3:1319:8a2e:0370:7347", "1:2:3:4:5:6:7:8:9",
      "g011:0db8:85a3:08d3:1319:8a2e:0370:7347:7347", "0001.0002.0003.0004", "0:0:0:0:0:0:0:0:", "0:0:0:0:0:0:0:",
      "0:0:0:0:0:1.2.3.4", "::ffff:001.02.03.004", "::ffff:1.2.3.1111", "::ffff:1.2.3.256", "::ffff:311.2.3.4",
      "::ffff:1.2.3:4", "::ffff:1.2.3", "::ffff:1.2.3.", "::ffff:1.2.3a.4", "::ffff:1.2.3.4:123",
      "2011:0db8:85a3:08d3:1319:8a2e:0370", "1:2:3:4:5:6", "1:2:3:4:5:6::8:9", ":2:3:4:5:6:7:8:9", "1:2:3:4:5:6:7:8:",
      "::2:3:4:5:6:7:8:9", "1:2:3:4:5:6:7:8::", "1:2:3:4:5:6:7:88888", "1:2:3:4:5:6:7:8:255.255.255.255",
      "1:2:3:4:5:255.255.255.255", "1:2:3:4:127.0.0.1::", "256.0.0.0", "260.0.0.0", "172.0.1", "10002.3.4.5",
      "1.2.3.4.5", "127.0.0.01"
    )

    listIn.foreach {
      x =>

        an[ParseError] should be thrownBy IpAddress(x)
    }
  }
}

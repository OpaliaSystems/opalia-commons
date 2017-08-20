package systems.opalia.commons.net

import org.scalatest._


class EndpointAddressTest
  extends FlatSpec
    with Matchers {

  it should "parse a valid list of IPv4 addresses" in {

    val listIn = List(
      "0.0.0.0:65535", "255.255.255.255:65535", "127.0.0.1:42", "10.0.0.1:42", "192.168.1.1:42")

    val listOut = listIn.map(EndpointAddress.parse)

    listIn.zip(listOut).foreach {
      x =>

        x._2.toString should be(x._1)
        x._2.hostType should be(HostType.IPv4)
    }
  }

  it should "throw an exception while parsing an invalid list of IPv4 addresses" in {

    val list = List(
      "256.0.0.0:42", "260.0.0.0:42", "255.255.255.255", "172.0.1", "10002.3.4.5", "1.2.3.4.5", "127.0.0.01:73",
      "255.255.255.255:65536")

    list.foreach {
      x =>

        an[IllegalArgumentException] should be thrownBy EndpointAddress.parse(x)
    }
  }

  it should "parse a valid list of IPv6 addresses" in {

    val list1 =
      List(
        "::", "0:0:0:0:0:0:0:0", "0000:0000:0000:0000:0000:0000:0000:0000", "2001:0db8:85a3:08d3:1319:8a2e:0370:73",
        "1:2:3:4:5:6:7:8", "::2:3:4:5:6:7:8", "1::3:4:5:6:7:8", "1:2::4:5:6:7:8", "1:2:3::5:6:7:8", "1:2:3:4::6:7:8",
        "1:2:3:4:5::7:8", "1:2:3:4:5:6::8", "1:2:3:4:5:6:7::", "1:2:3:4:5:6::", "1:2:3:4:5::", "1:2:3:4::", "1:2:3::",
        "1:2::", "1::", "::8", "1::8", "1:2::7:8", "fe08::7:8", "1:2:3:4:5:6:127.0.0.1", "1:2:3:4:5::127.0.0.1",
        "1:2:3::6:127.0.0.1", "::6:255.255.255.255", "::2:3:4:5:6:255.255.255.255", "::255.255.255.255",
        "::ffff:10.0.0.1")

    val list2 =
      (1 to 6)
        .map(x => (1 to x, 1 to (7 - x)))
        .flatMap(x => for (a <- x._1; b <- x._2) yield ("0:" * a) + (":0" * b))
        .distinct

    val list3 =
      for (x <- 1 to 7) yield ("0:" * x) + ":"

    val list4 =
      for (x <- 1 to 7) yield ":" + (":0" * x)

    val list5 =
      (1 to 4)
        .map(x => (1 to x, 1 to (5 - x)))
        .flatMap(x => for (a <- x._1; b <- x._2) yield ("0:" * a) + ":" + ("0:" * b) + "0.0.0.0")
        .distinct

    val list6 =
      for (x <- 1 to 5) yield ("0:" * x) + ":" + "0.0.0.0"

    val list7 =
      for (x <- 0 to 5) yield "::" + ("0:" * x) + "0.0.0.0"

    val listIn =
      (list1 ++ list2 ++ list3 ++ list4 ++ list5 ++ list6 ++ list7)
        .map("[" + _ + "]:73")

    val listOut = listIn.map(EndpointAddress.parse)

    listIn.zip(listOut).foreach {
      x =>

        x._2.toString should be(x._1)
        x._2.hostType should be(HostType.IPv6)
    }
  }

  it should "throw an exception while parsing an invalid list of IPv6 addresses" in {

    val list = List(
      ":::", ":::1234", "1234:::1234:1234", "1234:1234:::1234", "1234:::", "abcd::abcd::abcd",
      "20011:0db8:85a3:08d3:1319:8a2e:0370:7347", "::12345", "1:2:3:4:5:6:7:8:9", "0001.0002.0003.0004", "1.2.3.4",
      "0:0:0:0:0:0:0:0:", "0:0:0:0:0:0:0:", "0:0:0:0:0:1.2.3.4", "::ffff:001.02.03.004", "::ffff:1.2.3.1111",
      "::ffff:1.2.3.256", "::ffff:311.2.3.4", "::ffff:1.2.3:4", "::ffff:1.2.3", "::ffff:1.2.3.", "::ffff:1.2.3a.4",
      "::ffff:1.2.3.4:123", "2011:0db8:85a3:08d3:1319:8a2e:0370", "g011:0db8:85a3:08d3:1319:8a2e:0370:7347:7347",
      "1:2:3:4:5:6", "1:2:3:4:5:6::8:9", ":2:3:4:5:6:7:8:9", "1:2:3:4:5:6:7:8:", "::2:3:4:5:6:7:8:9",
      "1:2:3:4:5:6:7:8::", "1:2:3:4:5:6:7:88888", "1:2:3:4:5:6:7:8:255.255.255.255", "1:2:3:4:5:255.255.255.255",
      "1:2:3:4:127.0.0.1::")
      .map("[" + _ + "]:73")

    list.foreach {
      x =>

        an[IllegalArgumentException] should be thrownBy EndpointAddress.parse(x)
    }
  }

  it should "parse a valid list of hostnames" in {

    val listIn = List(
      "localhost",
      "local---host.tld",
      "foo.bar.baz.tld",
      "172.0.0.1.tld")
      .map(_ + ":42")

    val listOut = listIn.map(EndpointAddress.parse)

    listIn.zip(listOut).foreach {
      x =>

        x._2.toString should be(x._1)
        x._2.hostType should be(HostType.Hostname)
    }
  }

  it should "throw an exception while parsing an invalid list of hostnames" in {

    val list = List(
      "asdf@fdsa.tld",
      "asdf_fdsa.tld",
      "asdf..fdsa.tld",
      "asdf:fdsa.tld",
      "-localhost.tld",
      "localhost-.tld",
      "localhost.t-l-d",
      "localhost.tld2",
      "0123456789012345678901234567890123456789012345678901234567890123456789.tld")
      .map(_ + ":42")

    list.foreach {
      x =>

        an[IllegalArgumentException] should be thrownBy EndpointAddress.parse(x)
    }
  }
}

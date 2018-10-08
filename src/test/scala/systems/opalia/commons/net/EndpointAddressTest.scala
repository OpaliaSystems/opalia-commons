package systems.opalia.commons.net

import org.scalatest._


class EndpointAddressTest
  extends FlatSpec
    with Matchers {

  it should "parse a valid list of IP based endpoints" in {

    val listIn = List(
      ("0.0.0.0:65535", 65535), ("192.168.1.1:42", 42), ("172.0.0.1:0", 0),
      ("[::]:65535", 65535), ("[2001:0db8:85a3:08d3:1319:8a2e:0370:0073]:42", 42), ("[::ffff:10.0.0.1]:0", 0)
    )

    val listOut = listIn.map(x => EndpointAddress(x._1))

    listIn.zip(listOut).foreach {
      case ((origin, port), address) =>

        address.toString shouldBe origin
        address.port shouldBe port
    }
  }

  it should "throw an exception while parsing invalid ports" in {

    val list = List(
      "256.0.0.0:-1", "255.255.255.255", "255.255.255.255:65536",
      "[::]:-1", "1:2:3:4:5::7:8", "[2001:0db8:85a3:08d3:1319:8a2e:0370:73]", "[::ffff:10.0.0.1]:65536"
    )

    list.foreach {
      x =>

        an[IllegalArgumentException] should be thrownBy EndpointAddress(x)
    }
  }

  it should "parse a valid list of hostnames" in {

    val listIn = List(
      ("localhost:65535", 65535),
      ("local---host.tld:42", 42),
      ("foo.bar.baz.tld:73", 73),
      ("172.0.0.1.tld:0", 0)
    )

    val listOut = listIn.map(x => EndpointAddress(x._1))

    listIn.zip(listOut).foreach {
      case ((origin, port), address) =>

        address.toString shouldBe origin
        address.port shouldBe port
        address.host.right.get shouldBe origin.takeWhile(_ != ':')
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
      "localhost.tld.",
      ".localhost.tld",
      "0123456789012345678901234567890123456789012345678901234567890123456789.tld"
    )
      .map(_ + ":42")

    list.foreach {
      x =>

        an[IllegalArgumentException] should be thrownBy EndpointAddress(x)
    }
  }
}

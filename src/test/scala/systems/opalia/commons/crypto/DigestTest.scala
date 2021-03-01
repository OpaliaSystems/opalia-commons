package systems.opalia.commons.crypto

import org.scalatest.flatspec._
import org.scalatest.matchers.should._


class DigestTest
  extends AnyFlatSpec
    with Matchers {

  it should "sign MD5 with correct result" in {
    Digest(Digest.Algorithm.MD5).signToOctetString("this is a test") should
      be("54b0c58c7ce9f2a8b551351102ee0938")
  }

  it should "sign SHA-1 with correct result" in {
    Digest(Digest.Algorithm.SHA1).signToOctetString("this is a test") should
      be("fa26be19de6bff93f70bc2308434e4a440bbad02")
  }

  it should "sign SHA-256 with correct result" in {
    Digest(Digest.Algorithm.SHA256).signToOctetString("this is a test") should
      be("2e99758548972a8e8822ad47fa1017ff72f06f3ff6a016851f45c398732bc50c")
  }

  it should "sign SHA-384 with correct result" in {
    Digest(Digest.Algorithm.SHA384).signToOctetString("this is a test") should
      be("43382a8cc650904675c9d62d785786e368f3a99db99aeaaa7b76b02530677154d09c0b6bd2e21b4329fd41543b9a785b")
  }

  it should "sign SHA-512 with correct result" in {
    Digest(Digest.Algorithm.SHA512).signToOctetString("this is a test") should
      be("7d0a8468ed220400c0b8e6f335baa7e070ce880a37e2ac5995b9a97b809026de" +
        "626da636ac7365249bb974c719edf543b52ed286646f437dc7f810cc2068375c")
  }

  it should "sign MD5/HMAC with correct result" in {
    Digest(Digest.Algorithm.MD5, "secret key").signToOctetString("this is a test") should
      be("c51fe3378048cfdc356a902e2236e6b3")
  }

  it should "sign SHA-1/HMAC with correct result" in {
    Digest(Digest.Algorithm.SHA1, "secret key").signToOctetString("this is a test") should
      be("6cf81c2c82bb336d968bc6e3aba76f13ce5a3fe8")
  }

  it should "sign SHA-256/HMAC with correct result" in {
    Digest(Digest.Algorithm.SHA256, "secret key").signToOctetString("this is a test") should
      be("f3646df7b4ce412dd952bd3be01524237c7e18ef07ac286c2b19b97ef8d0e1f3")
  }

  it should "sign SHA-384/HMAC with correct result" in {
    Digest(Digest.Algorithm.SHA384, "secret key").signToOctetString("this is a test") should
      be("1cde591581192970893dffbde901094d726dafa7ae2395868e6a1803fac45bcba3b02b83b3426c32b08d3dc42ac9598a")
  }

  it should "sign SHA-512/HMAC with correct result" in {
    Digest(Digest.Algorithm.SHA512, "secret key").signToOctetString("this is a test") should
      be("16b03a48027ed92eb632371a42930ef7cab9827a72d6d7d55004612890ab2310" +
        "56e6e4fc95c80f2c88d2c2881ba590098abddcbec89e0a0e8f80e21fa3f481b9")
  }
}

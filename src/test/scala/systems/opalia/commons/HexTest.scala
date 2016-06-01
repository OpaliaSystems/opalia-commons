package systems.opalia.commons

import org.scalatest._
import systems.opalia.commons.codec.Hex


class HexTest
  extends FlatSpec
    with Matchers {

  it should "validate (42fdea) with success" in {
    Hex.isValid("42fdea") should be(true)
  }

  it should "validate (42fde) with failure" in {
    Hex.isValid("42fde") should be(false)
  }

  it should "validate (004Faf10) with success" in {
    Hex.isValid("004Faf10") should be(true)
  }

  it should "validate (42af73fg) with failure" in {
    Hex.isValid("42af73fg") should be(false)
  }

  it should "be able to generage a hex bubble string from byte array" in {
    Hex.encode(Array(
      0xbb, 0x74, 0x68, 0x69, 0x73,
      0x20, 0x69, 0x73, 0x20, 0x61,
      0x20, 0x74, 0x65, 0x73, 0x74,
      0xab).map(_.toByte)) should be("bb7468697320697320612074657374ab")
  }

  it should "be able to read a hex bubble string to generate a byte array" in {
    Hex.decode("bb6a75737420616e6f746865722074657374AB").map(_.toSeq) should be(Some(Array(
      0xbb, 0x6a, 0x75, 0x73, 0x74,
      0x20, 0x61, 0x6e, 0x6f, 0x74,
      0x68, 0x65, 0x72, 0x20, 0x74,
      0x65, 0x73, 0x74, 0xab).map(_.toByte).toSeq))
  }

  it should "be unable to read a string which is not a valid hex bubble" in {
    Hex.decode("bb6a75737420616e6f746865722074657374ax").map(_.toSeq) should be(None)
  }
}

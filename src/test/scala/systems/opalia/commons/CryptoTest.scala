package systems.opalia.commons

import org.scalatest._
import systems.opalia.commons.codec.Hex
import systems.opalia.commons.crypto.Crypto


class CryptoTest
  extends FlatSpec
    with Matchers {

  it should "sign MD5 with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.MD5,
      None
    )) should be("54b0c58c7ce9f2a8b551351102ee0938")
  }

  it should "sign SHA1 with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.SHA1,
      None
    )) should be("fa26be19de6bff93f70bc2308434e4a440bbad02")
  }

  it should "sign SHA256 with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.SHA256,
      None
    )) should be("2e99758548972a8e8822ad47fa1017ff72f06f3ff6a016851f45c398732bc50c")
  }

  it should "sign MD5/HMAC with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.MD5,
      Some("secret key")
    )) should be("c51fe3378048cfdc356a902e2236e6b3")
  }

  it should "sign SHA1/HMAC with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.SHA1,
      Some("secret key")
    )) should be("6cf81c2c82bb336d968bc6e3aba76f13ce5a3fe8")
  }

  it should "sign SHA256/HMAC with success for (this is a test)" in {
    Hex.encode(Crypto.sign(
      "this is a test",
      Crypto.SignAlgorithm.SHA256,
      Some("secret key")
    )) should be("f3646df7b4ce412dd952bd3be01524237c7e18ef07ac286c2b19b97ef8d0e1f3")
  }

  it should "encrypt/decrypt AES with success for (this is my secret message)" in {

    val key =
      "my_key"

    val encrypted =
      Crypto.encrypt("this is my secret message", key, Crypto.CipherAlgorithm.AES)

    val decrypted =
      Crypto.decrypt(encrypted, key, Crypto.CipherAlgorithm.AES)

    (new String(decrypted)) should be("this is my secret message")
  }

  it should "encrypt/decrypt AES with failure for (this is my secret message) with different keys" in {

    val (key1, key2) =
      ("my_key_1", "my_key_2")

    val encrypted =
      Crypto.encrypt("this is my secret message", key1, Crypto.CipherAlgorithm.AES)

    val decrypted =
      Crypto.decrypt(encrypted, key2, Crypto.CipherAlgorithm.AES)

    (new String(decrypted)) should not be "this is my secret message"
  }
}

package systems.opalia.commons.crypto

import java.io._
import java.nio.charset.Charset
import java.security.{SecureRandom, Security}
import javax.crypto.{CipherInputStream, Cipher => JCipher}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import systems.opalia.interfaces.rendering.Renderer


trait Cipher {

  val settings: CipherSettings

  def encrypt(data: String): IndexedSeq[Byte]

  def encrypt(data: String, charset: Charset): IndexedSeq[Byte]

  def encrypt(data: IndexedSeq[Byte]): IndexedSeq[Byte]

  def encrypt(data: InputStream): InputStream

  def encrypt(data: OutputStream): OutputStream

  def decrypt(data: IndexedSeq[Byte]): IndexedSeq[Byte]

  def decrypt(data: InputStream): InputStream

  def decrypt(data: OutputStream): OutputStream
}

object Cipher {

  def apply(settings: CipherSettings, secret: String): Cipher = {

    if (secret.isEmpty)
      throw new IllegalArgumentException("Expect non empty secret.")

    Security.addProvider(new BouncyCastleProvider())

    create(settings, secret)
  }

  private def create(_settings: CipherSettings, secret: String): Cipher = {

    new Cipher {

      private val random = new SecureRandom()

      val settings: CipherSettings = _settings

      def encrypt(data: String): IndexedSeq[Byte] =
        encrypt(data, Renderer.appDefaultCharset)

      def encrypt(data: String, charset: Charset): IndexedSeq[Byte] =
        encrypt(data.getBytes(charset))

      def encrypt(data: IndexedSeq[Byte]): IndexedSeq[Byte] = {

        val outputStream = new ByteArrayOutputStream()
        val outputStreamCypher = encrypt(outputStream)

        outputStreamCypher.write(data.toArray)
        outputStreamCypher.flush()
        outputStreamCypher.close()

        outputStream.toByteArray
      }

      def encrypt(data: InputStream): InputStream = {

        val salt = new Array[Byte](_settings.keySaltLength)
        val iv = new Array[Byte](_settings.ivLength)
        val handler = createHandler(enc = true, salt, iv)

        new SequenceInputStream(
          new ByteArrayInputStream(salt ++ iv),
          new CipherInputStream(data, handler)
        )
      }

      def encrypt(data: OutputStream): OutputStream = {

        val salt = new Array[Byte](_settings.keySaltLength)
        val iv = new Array[Byte](_settings.ivLength)
        val handler = createHandler(enc = true, salt, iv)

        data.write(salt)
        data.write(iv)

        new CipherOutputStream(data, handler)
      }

      def decrypt(data: IndexedSeq[Byte]): IndexedSeq[Byte] = {

        val outputStream = new ByteArrayOutputStream()
        val outputStreamCypher = decrypt(outputStream)

        outputStreamCypher.write(data.toArray)
        outputStreamCypher.flush()
        outputStreamCypher.close()

        outputStream.toByteArray
      }

      def decrypt(data: InputStream): InputStream = {

        val salt = new Array[Byte](_settings.keySaltLength)
        val iv = new Array[Byte](_settings.ivLength)

        val saltRead = data.read(salt)
        val ivRead = data.read(iv)

        if (saltRead != salt.length || ivRead != iv.length)
          throw new IllegalArgumentException("Expect more bytes on the input stream to extract initial parameters.")

        new CipherInputStream(data, createHandler(enc = false, salt, iv))
      }

      def decrypt(data: OutputStream): OutputStream = {

        new OutputStream {

          private var out: OutputStream =
            new OutputStream {

              private val aad = new Array[Byte](_settings.keySaltLength + _settings.ivLength)
              private var index = 0

              override def write(value: Int): Unit = {

                if (index < aad.length) {

                  aad(index) = value.toByte

                  index += 1

                } else {

                  if (out == this) {

                    val salt = aad.slice(0, _settings.keySaltLength)
                    val iv = aad.slice(_settings.keySaltLength, _settings.keySaltLength + _settings.ivLength)

                    val handler = createHandler(enc = false, salt, iv)

                    out = new CipherOutputStream(data, handler)
                  }

                  out.write(value)
                }
              }
            }

          override def write(value: Int): Unit = {

            out.write(value)
          }

          override def write(bytes: Array[Byte]): Unit = {

            out.write(bytes)
          }

          override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {

            out.write(bytes, offset, length)
          }

          override def flush(): Unit = {

            out.flush()
          }

          override def close(): Unit = {

            out.close()
          }
        }
      }

      private def createHandler(enc: Boolean, salt: Array[Byte], iv: Array[Byte]): JCipher = {

        if (enc) {

          random.nextBytes(salt)
          random.nextBytes(iv)
        }

        val handler = JCipher.getInstance(_settings.transformation, "BC")

        handler.init(
          if (enc) JCipher.ENCRYPT_MODE else JCipher.DECRYPT_MODE,
          _settings.createSecretKeySpec(secret, salt),
          _settings.createAlgorithmParameterSpec(iv)
        )

        if (_settings.supportAEAD)
          handler.updateAAD(salt ++ iv)

        handler
      }
    }
  }
}

package systems.opalia.commons.crypto

import java.io.OutputStream
import javax.crypto.{Cipher => JCipher}


class CipherOutputStream private[crypto](outputStream: OutputStream, handler: JCipher)
  extends OutputStream {

  override def write(value: Int): Unit = {

    val buffer = new Array[Byte](1)

    buffer(0) = value.toByte

    val result = handler.update(buffer)

    if (result != null)
      outputStream.write(result)
  }

  override def write(bytes: Array[Byte]): Unit = {

    val result = handler.update(bytes)

    if (result != null)
      outputStream.write(result)
  }

  override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {

    val result = handler.update(bytes, offset, length)

    if (result != null)
      outputStream.write(result)
  }

  override def flush(): Unit = {

    outputStream.flush()
  }

  override def close(): Unit = {

    val result = handler.doFinal()

    if (result != null)
      outputStream.write(result)

    outputStream.flush()
    outputStream.close()
  }
}

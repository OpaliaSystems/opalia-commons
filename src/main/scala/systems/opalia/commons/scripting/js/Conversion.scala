package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.Value
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyExecutable, ProxyObject}
import systems.opalia.interfaces.scripting._


private[js] trait Conversion {

  protected def prepareForPolyglot(value: Any): AnyRef = {

    value match {
      case array: ImmutableArrayProxy => new InternalImmutableProxyArray(array)
      case array: MutableArrayProxy => new InternalMutableProxyArray(array)
      case map: ImmutableMapProxy => new InternalImmutableProxyObject(map)
      case map: MutableMapProxy => new InternalMutableProxyObject(map)
      case executable: ExecutableProxy => new InternalProxyExecutable(executable)
      case scriptValue: ScriptValue => scriptValue.asInstanceOf[JsScriptValue].underlyingValue
      case other => other.asInstanceOf[AnyRef]
    }
  }

  private def indexToInt(index: Long): Int = {

    if (index > Int.MaxValue || index < 0)
      throw new IndexOutOfBoundsException("The index is out of bounds.")

    index.toInt
  }

  protected class InternalImmutableProxyArray(array: ImmutableArrayProxy)
    extends ProxyArray {

    def underlying: ImmutableArrayProxy =
      array

    override def get(index: Long): AnyRef =
      array(indexToInt(index)).asInstanceOf[AnyRef]

    override def set(index: Long, value: Value): Unit =
      throw new UnsupportedOperationException("Cannot set a value for an immutable proxy array.")

    override def getSize: Long =
      array.length
  }

  protected class InternalMutableProxyArray(array: MutableArrayProxy)
    extends ProxyArray {

    def underlying: MutableArrayProxy =
      array

    override def get(index: Long): AnyRef =
      array(indexToInt(index)).asInstanceOf[AnyRef]

    override def set(index: Long, value: Value): Unit =
      array.update(indexToInt(index), value)

    override def getSize: Long =
      array.length
  }

  protected class InternalImmutableProxyObject(map: ImmutableMapProxy)
    extends ProxyObject {

    def underlying: ImmutableMapProxy =
      map

    override def getMember(key: String): AnyRef =
      map(key).asInstanceOf[AnyRef]

    override def getMemberKeys: AnyRef =
      map.keys.toArray

    override def hasMember(key: String): Boolean =
      map.contains(key)

    override def putMember(key: String, value: Value): Unit =
      throw new UnsupportedOperationException("Cannot set a key-value pair for an immutable proxy map.")

    override def removeMember(key: String): Boolean =
      throw new UnsupportedOperationException("Cannot remove a key-value pair from an immutable proxy map.")
  }

  protected class InternalMutableProxyObject(map: MutableMapProxy)
    extends ProxyObject {

    def underlying: MutableMapProxy =
      map

    override def getMember(key: String): AnyRef =
      map(key).asInstanceOf[AnyRef]

    override def getMemberKeys: AnyRef =
      map.keys.toArray

    override def hasMember(key: String): Boolean =
      map.contains(key)

    override def putMember(key: String, value: Value): Unit =
      map.put(key, value)

    override def removeMember(key: String): Boolean =
      map.remove(key).isDefined
  }

  protected class InternalProxyExecutable(executable: ExecutableProxy)
    extends ProxyExecutable {

    def underlying: ExecutableProxy =
      executable

    def execute(arguments: Value*): AnyRef =
      executable(arguments.map(x => new JsScriptValue(x))).asInstanceOf[AnyRef]
  }

}

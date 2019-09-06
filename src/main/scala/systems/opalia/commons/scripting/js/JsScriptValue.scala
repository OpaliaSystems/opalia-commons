package systems.opalia.commons.scripting.js

import org.graalvm.polyglot.{PolyglotException, Value}
import scala.collection.JavaConverters._
import systems.opalia.interfaces.scripting._


final class JsScriptValue private[js](private[js] val underlyingValue: Value)
  extends ScriptValue
    with Conversion {

  override def toString: String = {

    underlyingValue.toString
  }

  def hasArrayElements: Boolean = {

    try {

      underlyingValue.hasArrayElements

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def getArrayElement(index: Int): ScriptValue = {

    try {

      new JsScriptValue(underlyingValue.getArrayElement(index))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def setArrayElement(index: Int, value: Any): Unit = {

    try {

      underlyingValue.setArrayElement(index, prepareForPolyglot(value))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def removeArrayElement(index: Int): Boolean = {

    try {

      underlyingValue.removeArrayElement(index)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def getArraySize: Int = {

    try {

      if (underlyingValue.getArraySize > Int.MaxValue)
        throw new IllegalArgumentException("The underlying array is too long.")

      underlyingValue.getArraySize.toInt

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def hasMembers: Boolean = {

    try {

      underlyingValue.hasMembers

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def hasMember(key: String): Boolean = {

    try {

      underlyingValue.hasMember(key)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def getMember(key: String): ScriptValue = {

    try {

      if (!underlyingValue.hasMember(key))
        throw new IllegalArgumentException(s"Cannot find member with key $key.")

      new JsScriptValue(underlyingValue.getMember(key))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def getMemberKeys: Set[String] = {

    try {

      underlyingValue.getMemberKeys.asScala.toSet

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def putMember(key: String, value: Any): Unit = {

    try {

      underlyingValue.putMember(key, prepareForPolyglot(value))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def removeMember(key: String): Boolean = {

    try {

      underlyingValue.removeMember(key)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def getMetaObject: ScriptValue = {

    try {

      new JsScriptValue(underlyingValue.getMetaObject)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def canExecute: Boolean = {

    try {

      underlyingValue.canExecute

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def execute(arguments: Any*): ScriptValue = {

    try {

      new JsScriptValue(underlyingValue.execute(arguments.map(prepareForPolyglot): _*))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def canInstantiate: Boolean = {

    try {

      underlyingValue.canInstantiate

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def newInstance(arguments: Any*): ScriptValue = {

    try {

      new JsScriptValue(underlyingValue.newInstance(arguments.map(prepareForPolyglot): _*))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def canInvokeMember(key: String): Boolean = {

    try {

      underlyingValue.canInvokeMember(key)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def invokeMember(key: String, arguments: Any*): ScriptValue = {

    try {

      new JsScriptValue(underlyingValue.invokeMember(key, arguments.map(prepareForPolyglot): _*))

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def isNull: Boolean = {

    try {

      underlyingValue.isNull

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def isBoolean: Boolean = {

    try {

      underlyingValue.isBoolean

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def isString: Boolean = {

    try {

      underlyingValue.isString

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def isNumber: Boolean = {

    try {

      underlyingValue.isNumber

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInByte: Boolean = {

    try {

      underlyingValue.fitsInByte

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInShort: Boolean = {

    try {

      underlyingValue.fitsInShort

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInInt: Boolean = {

    try {

      underlyingValue.fitsInInt

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInLong: Boolean = {

    try {

      underlyingValue.fitsInLong

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInFloat: Boolean = {

    try {

      underlyingValue.fitsInFloat

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def fitsInDouble: Boolean = {

    try {

      underlyingValue.fitsInDouble

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asBoolean: Boolean = {

    try {

      underlyingValue.asBoolean

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asString: String = {

    try {

      underlyingValue.asString

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asByte: Byte = {

    try {

      underlyingValue.asByte

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asShort: Short = {

    try {

      underlyingValue.asShort

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asInt: Int = {

    try {

      underlyingValue.asInt

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asLong: Long = {

    try {

      underlyingValue.asLong

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asFloat: Float = {

    try {

      underlyingValue.asFloat

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asDouble: Double = {

    try {

      underlyingValue.asDouble

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def isForeignObject: Boolean = {

    try {

      underlyingValue.isHostObject

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def asForeignObject: AnyRef = {

    try {

      underlyingValue.asHostObject

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }

  def as[T](clazz: Class[T]): T = {

    try {

      underlyingValue.as(clazz)

    } catch {

      case e: PolyglotException =>
        throw new ScriptException(e.getMessage, e)
    }
  }
}

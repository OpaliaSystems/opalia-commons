package systems.opalia.commons.scripting.calculator

import java.util.Objects
import systems.opalia.commons.scripting.calculator.FunctionDef.OperatorEntry


abstract class FunctionDef private(val parent: Option[FunctionDef],
                                   val signature: FunctionDef.Signature) {

  def globalFunctions: Set[FunctionDef]

  def globalOperators: Set[FunctionDef.OperatorEntry]

  override def equals(that: Any): Boolean =
    that match {

      case that: FunctionDef if (this.signature == that.signature) => true
      case _ => false
    }

  override def toString: String =
    parent.map(_.toString + " ~ ").getOrElse("") + signature.toString

  override def hashCode: Int =
    signature.hashCode

  private def internScope: Map[String, FunctionDef.Signature] = {

    val _1 =
      parent.map(_.internScope).getOrElse(Map.empty)

    val _2 =
      signature.parameters.map(x => (x.descriptor, x))

    val _3 =
      if (signature.descriptor.nonEmpty)
        Map(signature.descriptor -> signature)
      else
        Map.empty

    _1 ++ _2 ++ _3
  }

  def currentScope: Map[String, FunctionDef.Signature] =
    globalScope ++ internScope

  def globalScope: Map[String, FunctionDef.Signature] =
    globalFunctions.map(x => (x.signature.descriptor, x.signature)).toMap

  def createChild(signature: FunctionDef.Signature): FunctionDef = {

    val _parent = this

    new FunctionDef(Some(_parent), signature) {

      def globalFunctions: Set[FunctionDef] =
        _parent.globalFunctions

      def globalOperators: Set[OperatorEntry] =
        _parent.globalOperators
    }
  }
}

object FunctionDef {

  def root(signature: Signature,
           getGlobalFunctions: () => Set[FunctionDef],
           getGlobalOperators: () => Set[OperatorEntry]): FunctionDef = {

    new FunctionDef(None, signature) {

      def globalFunctions: Set[FunctionDef] =
        getGlobalFunctions.apply

      def globalOperators: Set[FunctionDef.OperatorEntry] =
        getGlobalOperators.apply
    }
  }

  def primitiveSignature: Signature =
    Signature("", Nil, None)

  case class Signature(descriptor: String,
                       parameters: List[Signature],
                       target: Option[Signature]) {

    if (!distinct())
      throw new CalculatorFormatException(s"Names of signature parts must be distinct.")

    if (target.exists(x => x.parameters.isEmpty && x.target.isEmpty))
      throw new CalculatorFormatException("Expect parameters or target for signature target part.")

    override def equals(that: Any): Boolean =
      that match {

        case that: Signature if (this.descriptor.nonEmpty && this.descriptor == that.descriptor) => true
        case _ => false
      }

    override def toString: String = {

      val descriptorValue =
        if (descriptor.isEmpty)
          "<λ>"
        else
          descriptor

      if (parameters.isEmpty && target.isEmpty)
        descriptorValue
      else
        "(" + descriptorValue +
          parameters.map(" " + _.toString).mkString +
          target.map(" \\ " + _.toString).getOrElse("") + ")"
    }

    override def hashCode: Int =
      descriptor.hashCode

    def verify(that: Signature): Boolean = {

      def walkTarget(p1: Option[Signature], p2: Option[Signature]): Boolean =
        (p1, p2) match {

          case (Some(x), Some(y)) =>
            walkParameters(x.parameters, y.parameters) && walkTarget(x.target, y.target)
          case (None, None) => true
          case _ => false
        }

      def walkParameters(p1: List[Signature], p2: List[Signature]): Boolean =
        (p1, p2) match {

          case (x :: xs, y :: ys) =>
            walkParameters(x.parameters, y.parameters) && walkParameters(xs, ys) && walkTarget(x.target, y.target)
          case (Nil, Nil) => true
          case _ => false
        }

      walkParameters(this.parameters, that.parameters) && walkTarget(this.target, that.target)
    }

    private def distinct(): Boolean = {

      parameters.forall(x => x.descriptor.isEmpty || x.descriptor != descriptor && x.distinct()) &&
        target.forall(x => x.descriptor.isEmpty || x.descriptor != descriptor && x.distinct())
    }
  }

  class OperatorEntry(val descriptor: String, val operator: String, val priority: Int) {

    def this(descriptor: String, operator: String) =
      this(descriptor, operator, 0)

    def this(descriptor: String, operator: String, priority: Option[Int]) =
      this(descriptor, operator, priority.getOrElse(0))

    override def equals(that: Any): Boolean =
      that match {

        case that: OperatorEntry if (this.descriptor == that.descriptor && this.operator == that.operator) => true
        case _ => false
      }

    override def hashCode: Int =
      Objects.hash(descriptor, operator)
  }

}

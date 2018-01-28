package systems.opalia.commons.scripting.calculator


abstract class FunctionDef private(val parent: Option[FunctionDef],
                                   val signature: FunctionDef.Signature) {

  def globalFunctions: Set[FunctionDef]

  override def equals(that: Any): Boolean =
    that match {

      case that: FunctionDef if (this.signature == that.signature) => true
      case _ => false
    }

  override def toString: String =
    parent.map(_.toString + " ~ ").getOrElse("") + signature.toString

  override def hashCode: Int =
    signature.hashCode

  private def currentScope: Map[String, FunctionDef.Signature] = {

    val _1 =
      parent.map(_.currentScope).getOrElse(Map.empty)

    val _2 =
      signature.parameters.map(x => (x.descriptor, x))

    val _3 =
      if (signature.descriptor.nonEmpty)
        Map(signature.descriptor -> signature)
      else
        Map.empty

    _1 ++ _2 ++ _3
  }

  def scope: Map[String, FunctionDef.Signature] = {

    val _1 =
      globalFunctions.map(x => (x.signature.descriptor, x.signature)).toMap

    val _2 =
      currentScope

    _1 ++ _2
  }

  def createChild(signature: FunctionDef.Signature): FunctionDef = {

    val _parent = this

    new FunctionDef(Some(_parent), signature) {

      def globalFunctions: Set[FunctionDef] =
        _parent.globalFunctions
    }
  }
}

object FunctionDef {

  def root(signature: Signature, getGlobalFunctions: () => Set[FunctionDef]): FunctionDef = {

    new FunctionDef(None, signature) {

      def globalFunctions = getGlobalFunctions()
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

}

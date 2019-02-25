package systems.opalia.commons.scripting.calculator


object Ast {

  sealed trait Node {

    val signature: FunctionDef.Signature
  }

  case class Body(function: FunctionDef, node: Node)

  case class LambdaFunctionNode(body: Body)
    extends Node {

    val signature: FunctionDef.Signature =
      body.function.signature
  }

  case class FunctionNode(descriptor: String, signature: FunctionDef.Signature)
    extends Node {
  }

  case class ApplicationNode(arguments: List[Node], node: Node)
    extends Node {

    val signature: FunctionDef.Signature =
      node.signature.target.getOrElse(FunctionDef.primitiveSignature)
  }

  case class NumberNode(number: String)
    extends Node {

    val signature: FunctionDef.Signature =
      FunctionDef.primitiveSignature
  }

}

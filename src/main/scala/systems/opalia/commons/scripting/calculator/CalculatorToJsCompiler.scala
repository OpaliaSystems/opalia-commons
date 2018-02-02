package systems.opalia.commons.scripting.calculator

import systems.opalia.commons.application.SystemProperty
import systems.opalia.commons.codec.Hex

class CalculatorToJsCompiler {

  def encodeName(descriptor: String): String =
    "_" + Hex.encode(descriptor.getBytes(SystemProperty.defaultCharset))

  def compile(body: Ast.Body): Vector[Char] = {

    def fold(seq: Seq[Vector[Char]]): Vector[Char] = {

      if (seq.isEmpty)
        Vector.empty[Char]
      else
        seq.reduce((a, b) => a ++ "," ++ b)
    }

    def transform(node: Ast.Node): Vector[Char] =
      node match {

        case node: Ast.LambdaFunctionNode => {

          Vector.empty[Char] ++
            compile(node.body)
        }

        case node: Ast.FunctionNode => {

          Vector.empty[Char] ++
            encodeName(node.descriptor)
        }

        case node: Ast.ApplicationNode => {

          Vector.empty[Char] ++
            transform(node.node) ++
            """(""" ++
            fold(node.arguments.map {

              case x@(_: Ast.ApplicationNode | _: Ast.NumberNode)
                if (x.signature.verify(FunctionDef.primitiveSignature)) =>
                Vector.empty[Char] ++
                  """(function(){return """ ++
                  transform(x) ++
                  """;})"""

              case x =>
                transform(x)

            }) ++
            """)"""
        }

        case node: Ast.NumberNode => {

          Vector.empty[Char] ++
            """(function(){return """ ++
            (if (node.negative) "-" else "") ++
            node.number ++
            """;})"""
        }
      }

    if (body.function.signature.descriptor.nonEmpty) {

      Vector.empty[Char] ++
        """function """ ++
        encodeName(body.function.signature.descriptor) ++
        """(""" ++
        fold(body.function.signature.parameters.map(x => encodeName(x.descriptor).toVector)) ++
        """){return """ ++
        transform(body.node) ++
        """;}"""

    } else {

      Vector.empty[Char] ++
        """(function(""" ++
        fold(body.function.signature.parameters.map(x => encodeName(x.descriptor).toVector)) ++
        """){return """ ++
        transform(body.node) ++
        """;})"""
    }
  }
}

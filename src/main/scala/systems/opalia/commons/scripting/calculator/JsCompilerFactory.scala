package systems.opalia.commons.scripting.calculator

import systems.opalia.commons.codec.Hex
import systems.opalia.interfaces.rendering._


private[calculator] final class JsCompilerFactory()
  extends CompilerFactory {

  def encodeName(descriptor: String): String =
    "_" + Hex.encode(descriptor.getBytes(Renderer.defaultCharset))

  def newCompiler(ast: Ast.Body): Compiler =
    new JsCompiler(ast)

  private class JsCompiler(val ast: Ast.Body)
    extends Compiler {

    def renderString(renderer: StringRenderer): StringRenderer = {

      def compile(body: Ast.Body): StringRenderer = {

        def joinComma(seq: Seq[StringRenderer]): StringRenderer =
          renderer.newEmpty.glue(seq, ",")

        def transform(node: Ast.Node): StringRenderer =
          node match {

            case node: Ast.LambdaFunctionNode => {

              compile(node.body)
            }

            case node: Ast.FunctionNode => {

              renderer.newEmpty ~ encodeName(node.descriptor)
            }

            case node: Ast.ApplicationNode => {

              renderer.newEmpty ~
                transform(node.node) ~
                """(""" ~
                joinComma(node.arguments.map {

                  case x@(_: Ast.ApplicationNode | _: Ast.NumberNode)
                    if (x.signature.verify(FunctionDef.primitiveSignature)) =>
                    renderer.newEmpty ~
                      """(function(){return """ ~
                      transform(x) ~
                      """;})"""

                  case x =>
                    transform(x)

                }) ~
                """)"""
            }

            case node: Ast.NumberNode => {

              renderer.newEmpty ~
                """(function(){return """ ~
                node.number ~
                """;})"""
            }
          }

        if (body.function.signature.descriptor.nonEmpty) {

          renderer.newEmpty ~
            """function """ ~
            encodeName(body.function.signature.descriptor) ~
            """(""" ~
            joinComma(body.function.signature.parameters.map(x => renderer.newEmpty ~ encodeName(x.descriptor))) ~
            """){return """ ~
            transform(body.node) ~
            """;}"""

        } else {

          renderer.newEmpty ~
            """(function(""" ~
            joinComma(body.function.signature.parameters.map(x => renderer.newEmpty ~ encodeName(x.descriptor))) ~
            """){return """ ~
            transform(body.node) ~
            """;})"""
        }
      }

      renderer ~ compile(ast)
    }
  }

}

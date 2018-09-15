package systems.opalia.commons.scripting.calculator

import systems.opalia.interfaces.rendering.StringRenderable


trait Compiler
  extends StringRenderable {

  val ast: Ast.Body
}

object Compiler {

  def newCompiler(factory: CompilerFactory, body: Ast.Body): Compiler = {

    factory.newCompiler(body)
  }
}

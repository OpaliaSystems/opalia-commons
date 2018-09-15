package systems.opalia.commons.scripting.calculator


trait Compiler {

  val ast: Ast.Body
}

object Compiler {

  def newCompiler(factory: CompilerFactory, body: Ast.Body): Compiler = {

    factory.newCompiler(body)
  }
}

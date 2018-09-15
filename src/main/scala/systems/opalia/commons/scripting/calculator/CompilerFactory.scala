package systems.opalia.commons.scripting.calculator


trait CompilerFactory {

  def encodeName(descriptor: String): String

  def newCompiler(body: Ast.Body): Compiler
}

object CompilerFactory {

  def newCompilerFactory(compilerType: CompilerType.Value): CompilerFactory = {

    compilerType match {
      case CompilerType.JavaScript => new JsCompilerFactory()
    }
  }

  object CompilerType
    extends Enumeration {

    val JavaScript = Value
  }

}

package systems.opalia.commons.scripting.calculator


trait CompilerFactory {

  def encodeName(descriptor: String): String

  def newCompiler(body: Ast.Body): Compiler
}

object CompilerFactory {

  def newCompilerFactory(compilerType: CompilerType): CompilerFactory = {

    compilerType match {
      case CompilerType.JavaScript => new JsCompilerFactory()
    }
  }

  sealed trait CompilerType

  object CompilerType {

    case object JavaScript
      extends CompilerType

  }

}

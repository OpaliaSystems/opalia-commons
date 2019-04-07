package systems.opalia.commons.scripting.calculator

import org.parboiled2._
import scala.util.{Failure, Success, Try}


class CalculatorParser {

  private object ParserAst {

    sealed trait Node

    sealed trait Declaration

    case class Body(nodes: List[Node])

    case class DeclarationFunction(signature: FunctionDef.Signature, body: Body)
      extends Declaration

    case class DeclarationOperator(operator: FunctionDef.OperatorEntry)
      extends Declaration

    case class LambdaFunctionNode(signature: FunctionDef.Signature, body: Body, leadingSpace: Boolean)
      extends Node

    case class FunctionNode(descriptor: String, leadingSpace: Boolean)
      extends Node

    case class OperatorNode(operator: String, leadingSpace: Boolean)
      extends Node

    case class BlockNode(nodes: List[Node], leadingSpace: Boolean)
      extends Node

    case class NumberNode(number: String, leadingSpace: Boolean)
      extends Node

  }

  private class FirstStepParser(val input: ParserInput)
    extends Parser {

    private val Whitespace = CharPredicate.from(x => x.isWhitespace)
    private val GreekLowerAlpha = CharPredicate('α' to 'ω')
    private val GreekUpperAlpha = CharPredicate('Α' to 'Ω')
    private val GreekAlpha = GreekLowerAlpha ++ GreekUpperAlpha
    private val AllAlpha = GreekAlpha ++ CharPredicate.Alpha ++ '∞' ++ 'Ø'
    private val DescriptorTailChars = AllAlpha ++ CharPredicate.Digit ++ "_',"
    private val Operator = CharPredicate("⊕+⊖-±∓⊗×*÷/∘%‰°^≀~≈≅≡≜≐=≠<>≤≥≦≧≪≫≺≻?!&@|∤∩∪⊂⊃⊆⊇⊄⊅⇐⇒←→⇔↔¬⊻⋉⋊⋈∧∨∀∃")

    def `descriptor`: Rule1[String] =
      rule {

        capture(AllAlpha ~ zeroOrMore(DescriptorTailChars))
      }

    def `descriptor-expression`: Rule1[String] =
      rule {

        `descriptor` ~ EOI
      }

    def `operator`: Rule1[String] =
      rule {

        capture(oneOrMore(Operator))
      }

    def `declaration`: Rule1[ParserAst.Declaration] =
      rule {

        `declaration-function` | `declaration-operator`
      }

    def `declaration-function`: Rule1[ParserAst.DeclarationFunction] =
      rule {

        zeroOrMore(Whitespace) ~ `descriptor` ~
          zeroOrMore(oneOrMore(Whitespace) ~ `signature-part`) ~
          optional(oneOrMore(Whitespace) ~ '\\' ~ oneOrMore(Whitespace) ~ `signature-part`) ~
          oneOrMore(Whitespace) ~ ':' ~ oneOrMore(Whitespace) ~ `body` ~
          zeroOrMore(Whitespace) ~> {
          (descriptor: String,
           parameters: Seq[FunctionDef.Signature],
           target: Option[FunctionDef.Signature],
           body: ParserAst.Body) =>

            ParserAst.DeclarationFunction(FunctionDef.Signature(descriptor, parameters.toList, target), body)
        }
      }

    def `declaration-operator`: Rule1[ParserAst.DeclarationOperator] =
      rule {

        zeroOrMore(Whitespace) ~ '{' ~ zeroOrMore(Whitespace) ~ `descriptor` ~ oneOrMore(Whitespace) ~ ':' ~
          oneOrMore(Whitespace) ~ `operator` ~ optional(oneOrMore(Whitespace) ~ `priority`) ~
          zeroOrMore(Whitespace) ~ '}' ~ zeroOrMore(Whitespace) ~> {
          (descriptor: String,
           operator: String,
           priority: Option[Int]) =>

            ParserAst.DeclarationOperator(new FunctionDef.OperatorEntry(descriptor, operator, priority))
        }
      }

    def `declaration-expression`: Rule1[ParserAst.Declaration] =
      rule {

        `declaration` ~ EOI
      }

    def `signature-part`: Rule1[FunctionDef.Signature] =
      rule {

        `signature-part-0` | `signature-part-N`
      }

    def `signature-part-0`: Rule1[FunctionDef.Signature] =
      rule {

        `descriptor` ~> ((descriptor: String) => FunctionDef.Signature(descriptor, Nil, None))
      }

    def `signature-part-N`: Rule1[FunctionDef.Signature] =
      rule {

        '(' ~
          zeroOrMore(Whitespace) ~ `descriptor` ~
          zeroOrMore(oneOrMore(Whitespace) ~ `signature-part`) ~
          optional(oneOrMore(Whitespace) ~ '\\' ~ oneOrMore(Whitespace) ~ `signature-part`) ~
          zeroOrMore(Whitespace) ~ ')' ~> {
          (descriptor: String,
           parameters: Seq[FunctionDef.Signature],
           target: Option[FunctionDef.Signature]) =>

            FunctionDef.Signature(descriptor, parameters.toList, target)
        }
      }

    def `body`: Rule1[ParserAst.Body] =
      rule {

        oneOrMore(`node`) ~> ((x: Seq[ParserAst.Node]) => ParserAst.Body(x.toList))
      }

    def `body-expression`: Rule1[ParserAst.Body] =
      rule {

        `body` ~ EOI
      }

    def `node`: Rule1[ParserAst.Node] =
      rule {

        `lambda-node` | `block-node` | `function-node` | `operator-node` | `number-node` | `operation-node`
      }

    def `lambda-node`: Rule1[ParserAst.LambdaFunctionNode] =
      rule {

        `leading-space` ~ '(' ~
          zeroOrMore(Whitespace) ~ oneOrMore(`signature-part`).separatedBy(oneOrMore(Whitespace)) ~
          optional(oneOrMore(Whitespace) ~ '\\' ~ oneOrMore(Whitespace) ~ `signature-part`) ~
          oneOrMore(Whitespace) ~ '.' ~ oneOrMore(Whitespace) ~ `body` ~
          zeroOrMore(Whitespace) ~ ')' ~> {
          (leadingSpace: Boolean,
           parameters: Seq[FunctionDef.Signature],
           target: Option[FunctionDef.Signature],
           body: ParserAst.Body) =>

            ParserAst.LambdaFunctionNode(FunctionDef.Signature("", parameters.toList, target), body, leadingSpace)
        }
      }

    def `block-node`: Rule1[ParserAst.BlockNode] =
      rule {

        `leading-space` ~ '(' ~ oneOrMore(`node`) ~ zeroOrMore(Whitespace) ~ ')' ~> {
          (leadingSpace: Boolean, nodes: Seq[ParserAst.Node]) =>

            ParserAst.BlockNode(nodes.toList, leadingSpace)
        }
      }

    def `function-node`: Rule1[ParserAst.FunctionNode] =
      rule {

        `leading-space` ~ `descriptor` ~> {
          (leadingSpace: Boolean, descriptor: String) =>

            ParserAst.FunctionNode(descriptor, leadingSpace)
        }
      }

    def `operator-node`: Rule1[ParserAst.OperatorNode] =
      rule {

        `leading-space` ~ `operator` ~> {
          (leadingSpace: Boolean, descriptor: String) =>

            ParserAst.OperatorNode(descriptor, leadingSpace)
        }
      }

    def `number-node`: Rule1[ParserAst.NumberNode] =
      rule {

        `leading-space` ~ capture(oneOrMore(CharPredicate.Digit) ~
          optional('.' ~ oneOrMore(CharPredicate.Digit)) ~
          optional(ignoreCase('e') ~ optional(anyOf("+-")) ~ oneOrMore(CharPredicate.Digit))
        ) ~> {
          (leadingSpace: Boolean, value: String) =>

            ParserAst.NumberNode(value, leadingSpace)
        }
      }

    def `operation-node`: Rule1[ParserAst.Node] =
      rule {

        `leading-space` ~ `operator` ~> {
          (leadingSpace: Boolean, operator: String) =>

            ParserAst.OperatorNode(operator, leadingSpace)
        }
      }

    def `priority`: Rule1[Int] =
      rule {

        capture((CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) | '0') ~>
          ((x: String) => BigInt(x)) ~> ((x: BigInt) => test(x <= 1000000) ~ push(x.toInt))
      }

    def `leading-space`: Rule1[Boolean] =
      rule {

        capture(zeroOrMore(Whitespace)) ~> ((x: String) => x.nonEmpty)
      }
  }

  private def parseDeclaration(value: String): ParserAst.Declaration = {

    val parser = new FirstStepParser(value)

    parser.`declaration-expression`.run() match {
      case Failure(e: ParseError) =>
        throw new CalculatorParserException(s"Failed to parse function declaration.\n${parser.formatError(e)}")
      case Failure(e) =>
        throw e
      case Success(x) =>
        x
    }
  }

  private def parseBody(value: String): ParserAst.Body = {

    val parser = new FirstStepParser(value)

    parser.`body-expression`.run() match {
      case Failure(e: ParseError) =>
        throw new CalculatorParserException(s"Failed to parse function body.\n${parser.formatError(e)}")
      case Failure(e) =>
        throw e
      case Success(x) =>
        x
    }
  }

  private def parseDescriptor(value: String): String = {

    val parser = new FirstStepParser(value)

    parser.`descriptor-expression`.run() match {
      case Failure(e: ParseError) =>
        throw new CalculatorParserException(s"Failed to parse function descriptor.\n${parser.formatError(e)}")
      case Failure(e) =>
        throw e
      case Success(x) =>
        x
    }
  }

  def validateDescriptor(descriptor: String): Boolean = {

    Try(parseDescriptor(descriptor)).isSuccess
  }

  def parse(string: String,
            addFunction: (FunctionDef) => Unit,
            addOperator: (FunctionDef.OperatorEntry) => Unit,
            getGlobalFunctions: () => Set[FunctionDef],
            getGlobalOperators: () => Set[FunctionDef.OperatorEntry]): List[Ast.Body] = {

    def group(lines: List[String], acc: String = ""): List[String] =
      lines match {
        case line :: rest if (line.indexOf('#') >= 0) =>
          group(line.slice(0, line.indexOf('#')) :: rest, acc)
        case line :: rest if (line.indexOf(';') >= 0) =>
          group(line.slice(0, line.indexOf(';')) :: Nil, acc) ++
            group(line.slice(line.indexOf(';') + 1, line.length) :: rest)
        case line :: rest if (line.indexOf(':') >= 0) =>
          acc :: group(rest, line)
        case line :: rest =>
          group(rest, acc + " " + line)
        case Nil =>
          acc :: Nil
      }

    val lines = string.split("\n").toList.map(_.takeWhile(_ != '#'))

    val declarations =
      group(lines)
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(parseDeclaration)
        .flatMap {
          case declaration: ParserAst.DeclarationFunction =>

            val function =
              FunctionDef.root(declaration.signature, () => getGlobalFunctions(), () => getGlobalOperators())

            addFunction(function)

            Some(function, declaration.body)

          case declaration: ParserAst.DeclarationOperator =>

            addOperator(declaration.operator)

            None
        }

    declarations.map {
      case (function, body) =>

        transform(function, body)
    }
  }

  def parseBody(string: String,
                signature: FunctionDef.Signature,
                addFunction: (FunctionDef) => Unit,
                getGlobalFunctions: () => Set[FunctionDef],
                getGlobalOperators: () => Set[FunctionDef.OperatorEntry]): Ast.Body = {

    val lines = string.split("\n").toList.map(_.takeWhile(_ != '#'))

    val body = parseBody(lines.mkString(" ").trim)

    val function = FunctionDef.root(signature, () => getGlobalFunctions(), () => getGlobalOperators())

    addFunction(function)

    transform(function, body)
  }

  private def transform(parentFunction: FunctionDef,
                        body: ParserAst.Body): Ast.Body = {

    sealed trait Node

    case class WrapperNode(node: Ast.Node, leadingSpace: Boolean)
      extends Node

    case class OperationNode(node: ParserAst.OperatorNode)
      extends Node

    val currentScope = parentFunction.currentScope
    val globalScope = parentFunction.globalScope
    val globalOperators = parentFunction.globalOperators

    def findSignature(descriptor: String): FunctionDef.Signature =
      currentScope.getOrElse(descriptor,
        throw new CalculatorParserException(
          s"Error in body of function $parentFunction.\n" +
            s"Cannot find function $descriptor in current scope."))

    def findSignatureGlobal(descriptor: String): FunctionDef.Signature =
      globalScope.getOrElse(descriptor,
        throw new CalculatorParserException(
          s"Error in body of function $parentFunction.\n" +
            s"Cannot find function $descriptor in global scope."))

    def findDescriptorByOperator(operator: String, operands: Int): String =
      globalOperators.find(x => x.operator == operator &&
        globalScope.get(x.descriptor).exists(_.parameters.length == operands))
        .map(_.descriptor)
        .getOrElse(throw new CalculatorParserException(
          s"Error in body of function $parentFunction.\n" +
            s"Cannot find function for operator chars {$operator}."))

    def verifySignature(descriptor: String,
                        signatureOfArg: FunctionDef.Signature,
                        signatureOfParam: FunctionDef.Signature,
                        position: Int): Unit =
      if (!signatureOfParam.verify(signatureOfArg))
        throw new CalculatorParserException(
          s"Error in body of function $parentFunction.\n" +
            s"Cannot apply argument $signatureOfArg at position $position on parameter $signatureOfParam" +
            s" of function $descriptor.")

    def applyArgumentsToOperator(descriptor: String, arguments: List[Ast.Node]): Ast.Node = {

      val signature = findSignatureGlobal(descriptor)

      if (signature.parameters.size != arguments.size ||
        !signature.parameters.forall(_.verify(FunctionDef.primitiveSignature)) ||
        signature.target.nonEmpty)
        throw new CalculatorRuntimeException(
          s"Error in body of function $parentFunction.\n" +
            s"Signature of function ${signature.descriptor} must have ${arguments.size} ${
              if (arguments.size == 1)
                "argument"
              else
                "arguments"
            } with primitive parameters and result.")

      arguments.zipWithIndex.foreach {
        case (operand, position) =>

          val parameter = signature.parameters(position)

          verifySignature(signature.descriptor, operand.signature, parameter, position + 1)
      }

      Ast.ApplicationNode(arguments, Ast.FunctionNode(descriptor, signature))
    }

    def getInnerNode(node: ParserAst.BlockNode): Ast.Node = {

      val innerNode = transformApplication(node.nodes)

      val applicable =
        (node.nodes, innerNode) match {
          case ((_: ParserAst.BlockNode) :: Nil, _: Ast.ApplicationNode) => true
          case (_, _: Ast.ApplicationNode) => false
          case _ => true
        }

      if (applicable && innerNode.signature.parameters.isEmpty && innerNode.signature.target.nonEmpty)
        Ast.ApplicationNode(Nil, innerNode)
      else
        innerNode
    }

    def transformApplication(nodes: List[ParserAst.Node]): Ast.Node = {

      val outer = nodes.size == 1

      def process(nodes: List[ParserAst.Node]): List[Node] =
        nodes match {

          case (node: ParserAst.LambdaFunctionNode) :: tail => {

            val function = parentFunction.createChild(node.signature)
            val body = transform(function, node.body)

            val (result, rest) =
              if (outer) {

                (Ast.LambdaFunctionNode(body), tail)

              } else {

                val (rest, arguments) = transformArguments(node.signature, tail)

                val result = Ast.ApplicationNode(arguments, Ast.LambdaFunctionNode(body))

                (result, rest)
              }

            WrapperNode(result, node.leadingSpace) :: process(rest)
          }

          case (node: ParserAst.FunctionNode) :: tail => {

            val signature = findSignature(node.descriptor)

            val (result, rest) =
              if (outer && !signature.verify(FunctionDef.primitiveSignature)) {

                (Ast.FunctionNode(node.descriptor, signature), tail)

              } else {

                val (rest, arguments) = transformArguments(signature, tail)

                val result = Ast.ApplicationNode(arguments, Ast.FunctionNode(node.descriptor, signature))

                (result, rest)
              }

            WrapperNode(result, node.leadingSpace) :: process(rest)
          }

          case (node: ParserAst.BlockNode) :: tail => {

            val innerNode = getInnerNode(node)

            val (result, rest) =
              if (outer || innerNode.signature.verify(FunctionDef.primitiveSignature)) {

                (innerNode, tail)

              } else {

                val (rest, arguments) = transformArguments(innerNode.signature, tail)

                val result = Ast.ApplicationNode(arguments, innerNode)

                (result, rest)
              }

            WrapperNode(result, node.leadingSpace) :: process(rest)
          }

          case (node: ParserAst.NumberNode) :: tail => {

            val result = Ast.NumberNode(node.number)

            WrapperNode(result, node.leadingSpace) :: process(tail)
          }

          case (node: ParserAst.OperatorNode) :: tail => {

            val result = OperationNode(node)

            result :: process(tail)
          }

          case Nil =>
            Nil
        }

      transformOperations(process(nodes))
    }

    def transformArguments(signature: FunctionDef.Signature,
                           rest: List[ParserAst.Node]): (List[ParserAst.Node], List[Ast.Node]) = {

      def handleLambdaFunctionNode(parameter: FunctionDef.Signature,
                                   position: Int,
                                   node: ParserAst.LambdaFunctionNode): Ast.Node = {

        val function = parentFunction.createChild(node.signature)
        val body = transform(function, node.body)

        val result = Ast.LambdaFunctionNode(body)

        verifySignature(signature.descriptor, parameter, result.signature, position)

        result
      }

      def handleFunctionNode(parameter: FunctionDef.Signature,
                             position: Int,
                             node: ParserAst.FunctionNode,
                             operator: Option[String]): Ast.Node = {

        val result = Ast.FunctionNode(node.descriptor, findSignature(node.descriptor))

        verifySignature(signature.descriptor, parameter, result.signature, position)

        operator.map(x => applyArgumentsToOperator(findDescriptorByOperator(x, 1), List(result))).getOrElse(result)
      }

      def handleBlockNode(parameter: FunctionDef.Signature,
                          position: Int,
                          node: ParserAst.BlockNode,
                          operator: Option[String]): Ast.Node = {

        val result = getInnerNode(node)

        verifySignature(signature.descriptor, parameter, result.signature, position)

        operator.map(x => applyArgumentsToOperator(findDescriptorByOperator(x, 1), List(result))).getOrElse(result)
      }

      def handleNumberNode(parameter: FunctionDef.Signature,
                           position: Int,
                           node: ParserAst.NumberNode,
                           operator: Option[String]): Ast.Node = {

        val result = Ast.NumberNode(node.number)

        verifySignature(signature.descriptor, parameter, result.signature, position)

        operator.map(x => applyArgumentsToOperator(findDescriptorByOperator(x, 1), List(result))).getOrElse(result)
      }

      def matchArguments(parameter: FunctionDef.Signature,
                         position: Int,
                         rest: List[ParserAst.Node]): (List[ParserAst.Node], Ast.Node) =
        rest match {

          case (node: ParserAst.LambdaFunctionNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleLambdaFunctionNode(parameter, position, node))

          case (node: ParserAst.FunctionNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleFunctionNode(parameter, position, node, None))

          case ParserAst.OperatorNode(operator, true) :: (node: ParserAst.FunctionNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleFunctionNode(parameter, position, node, Some(operator)))

          case (node: ParserAst.BlockNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleBlockNode(parameter, position, node, None))

          case ParserAst.OperatorNode(operator, true) :: (node: ParserAst.BlockNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleBlockNode(parameter, position, node, Some(operator)))

          case (node: ParserAst.NumberNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleNumberNode(parameter, position, node, None))

          case ParserAst.OperatorNode(operator, true) :: (node: ParserAst.NumberNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleNumberNode(parameter, position, node, Some(operator)))

          case Nil =>
            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Expect ${signature.parameters.length} ${
                  if (signature.parameters.length == 1)
                    "argument"
                  else
                    "arguments"
                } to apply function ${signature.descriptor}.")

          case _ =>
            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Syntax error with argument at position $position on parameter $parameter" +
                s" of function ${signature.descriptor}.")
        }

      def process(signature: List[FunctionDef.Signature],
                  position: Int,
                  rest: List[ParserAst.Node],
                  acc: List[Ast.Node]): (List[ParserAst.Node], List[Ast.Node]) =
        signature match {

          case head :: tail => {

            val (pending, node) = matchArguments(head, position, rest)

            process(tail, position + 1, pending, acc :+ node)
          }

          case Nil =>
            (rest, acc)
        }

      process(signature.parameters, 1, rest, Nil)
    }

    def transformOperations(nodes: List[Node]): Ast.Node = {

      def transformOperands(nodes: List[Node], operators: List[FunctionDef.OperatorEntry], first: Boolean = false): List[Node] =
        nodes match {

          case node :: Nil =>
            List(node)

          case OperationNode(op) :: (right@WrapperNode(node, false)) :: tail if (first) =>
            operators
              .find(x => x.operator == op.operator && globalScope.get(x.descriptor).exists(_.parameters.length == 1))
              .map {
                operator =>

                  val result = WrapperNode(applyArgumentsToOperator(operator.descriptor, List(node)), false)

                  transformOperands(result :: tail, operators)

              }.getOrElse {

              OperationNode(op) :: transformOperands(right :: tail, operators)
            }

          case (left@WrapperNode(node1, _)) :: OperationNode(op) :: (right@WrapperNode(node2, _)) :: tail =>
            operators
              .find(x => x.operator == op.operator && globalScope.get(x.descriptor).exists(_.parameters.length == 2))
              .map {
                operator =>

                  val result = WrapperNode(applyArgumentsToOperator(operator.descriptor, List(node1, node2)), false)

                  transformOperands(result :: tail, operators)

              }.getOrElse {

              left :: OperationNode(op) :: transformOperands(right :: tail, operators)
            }

          case (left@WrapperNode(node1, _)) :: OperationNode(op1) :: OperationNode(op2) :: (right@WrapperNode(node2, false)) :: tail =>
            operators
              .find(x => x.operator == op2.operator && globalScope.get(x.descriptor).exists(_.parameters.length == 1))
              .map {
                operator =>

                  val result = WrapperNode(applyArgumentsToOperator(operator.descriptor, List(node2)), false)

                  transformOperands(left :: OperationNode(op1) :: result :: tail, operators)

              }.getOrElse {

              operators
                .find(x => x.operator == op1.operator && globalScope.get(x.descriptor).exists(_.parameters.length == 2))
                .map {
                  operator =>

                    val node3 = applyArgumentsToOperator(findDescriptorByOperator(op2.operator, 1), List(node2))
                    val result = WrapperNode(applyArgumentsToOperator(operator.descriptor, List(node1, node3)), false)

                    transformOperands(result :: tail, operators)

                }.getOrElse {

                left :: OperationNode(op1) :: OperationNode(op2) :: transformOperands(right :: tail, operators)
              }
            }

          case Nil =>
            Nil
        }

      def process(nodes: List[Node]): Ast.Node = {

        val result =
          globalOperators.toSeq
            .groupBy(_.priority).toSeq
            .sortWith((a, b) => a._1 > b._1)
            .foldLeft(nodes)((a, b) => transformOperands(a, b._2.toList, first = true))

        result match {

          case WrapperNode(head, _) :: Nil =>
            head

          case Nil =>
            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Syntax error caused by empty function body.")

          case list => {

            val operators =
              list
                .flatMap {
                  case OperationNode(node) => Some(node)
                  case _ => None
                }
                .map(_.operator)

            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Cannot substitute all operators {${operators.mkString(", ")}}.")
          }
        }
      }

      process(nodes)
    }

    val outerNode = transformApplication(body.nodes)
    val targetSignature = parentFunction.signature.target.getOrElse(FunctionDef.primitiveSignature)

    if (!targetSignature.verify(outerNode.signature))
      throw new CalculatorParserException(
        s"Error in body of function $parentFunction.\n" +
          s"Cannot verify body signature ${outerNode.signature} with target signature $targetSignature" +
          s" of function ${parentFunction.signature.descriptor}.")

    Ast.Body(parentFunction, outerNode)
  }
}

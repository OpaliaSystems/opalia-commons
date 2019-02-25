package systems.opalia.commons.scripting.calculator

import org.parboiled2._
import scala.util.{Failure, Success, Try}


class CalculatorParser {

  private object Operator
    extends Enumeration {

    val Add = Value("add")
    val Sub = Value("sub")
    val Mul = Value("mul")
    val Div = Value("div")
    val Mod = Value("mod")
    val Pow = Value("pow")
    val Eq = Value("eq")
    val Ne = Value("ne")
    val Gt = Value("gt")
    val Ge = Value("ge")
    val Lt = Value("lt")
    val Le = Value("le")

    val Pos = "pos"
    val Neg = "neg"
  }

  private object ParserAst {

    sealed trait Node

    case class Body(nodes: List[Node])

    case class Declaration(signature: FunctionDef.Signature, body: Body)

    case class LambdaFunctionNode(signature: FunctionDef.Signature, body: Body, leadingSpace: Boolean)
      extends Node

    case class FunctionNode(descriptor: String, leadingSpace: Boolean)
      extends Node

    case class BlockNode(nodes: List[Node], leadingSpace: Boolean)
      extends Node

    case class NumberNode(number: String, leadingSpace: Boolean)
      extends Node

    case class OperationNode(operator: Operator.Value, leadingSpace: Boolean)
      extends Node

  }

  private class FirstStepParser(val input: ParserInput)
    extends Parser {

    private val Whitespace = CharPredicate.from(x => x.isWhitespace)
    private val GreekLowerAlpha = CharPredicate('α' to 'ω')
    private val GreekUpperAlpha = CharPredicate('Α' to 'Ω')
    private val GreekAlpha = GreekLowerAlpha ++ GreekUpperAlpha
    private val AllAlpha = GreekAlpha ++ CharPredicate.Alpha
    private val DescriptorTailChars = AllAlpha ++ CharPredicate.Digit ++ '_'

    def `descriptor`: Rule1[String] =
      rule {

        capture(AllAlpha ~ zeroOrMore(DescriptorTailChars))
      }

    def `descriptor-expression`: Rule1[String] =
      rule {

        `descriptor` ~ EOI
      }

    def `declaration`: Rule1[ParserAst.Declaration] =
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

            ParserAst.Declaration(FunctionDef.Signature(descriptor, parameters.toList, target), body)
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

        `lambda-node` | `block-node` | `function-node` | `number-node` | `operation-node`
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

    def `function-node`: Rule1[ParserAst.FunctionNode] =
      rule {

        `leading-space` ~ `descriptor` ~> {
          (leadingSpace: Boolean, descriptor: String) =>

            ParserAst.FunctionNode(descriptor, leadingSpace)
        }
      }

    def `block-node`: Rule1[ParserAst.BlockNode] =
      rule {

        `leading-space` ~ '(' ~ oneOrMore(`node`) ~ zeroOrMore(Whitespace) ~ ')' ~> {
          (leadingSpace: Boolean, nodes: Seq[ParserAst.Node]) =>

            ParserAst.BlockNode(nodes.toList, leadingSpace)
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
          (leadingSpace: Boolean, operator: Operator.Value) =>

            ParserAst.OperationNode(operator, leadingSpace)
        }
      }

    def `operator`: Rule1[Operator.Value] =
      rule {

        "+" ~ push(Operator.Add) |
          "-" ~ push(Operator.Sub) |
          "*" ~ push(Operator.Mul) |
          "×" ~ push(Operator.Mul) |
          "/" ~ push(Operator.Div) |
          "÷" ~ push(Operator.Div) |
          "%" ~ push(Operator.Mod) |
          "^" ~ push(Operator.Pow) |
          "<=" ~ push(Operator.Le) |
          "≤" ~ push(Operator.Le) |
          ">=" ~ push(Operator.Ge) |
          "≥" ~ push(Operator.Ge) |
          "<>" ~ push(Operator.Ne) |
          "≠" ~ push(Operator.Ne) |
          "=" ~ push(Operator.Eq) |
          "<" ~ push(Operator.Lt) |
          ">" ~ push(Operator.Gt)
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
            handleFunction: (FunctionDef) => Unit,
            getGlobalFunctions: () => Set[FunctionDef]): List[Ast.Body] = {

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

    val lines = string.split("\n").toList

    val declarations =
      group(lines)
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(parseDeclaration)
        .map {
          declaration =>

            val function = FunctionDef.root(declaration.signature, () => getGlobalFunctions())

            handleFunction(function)

            (function, declaration.body)
        }

    declarations.map {
      case (function, body) =>

        transform(function, body)
    }
  }

  def parseBody(string: String,
                signature: FunctionDef.Signature,
                handleFunction: (FunctionDef) => Unit,
                getGlobalFunctions: () => Set[FunctionDef]): Ast.Body = {

    val lines = string.split("\n").toList.map(_.takeWhile(_ != '#'))

    val body = parseBody(lines.mkString(" ").trim)

    val function = FunctionDef.root(signature, () => getGlobalFunctions())

    handleFunction(function)

    transform(function, body)
  }

  private def transform(parentFunction: FunctionDef,
                        body: ParserAst.Body): Ast.Body = {

    sealed trait Node

    case class WrapperNode(node: Ast.Node, leadingSpace: Boolean)
      extends Node

    case class OperationNode(operator: Operator.Value)
      extends Node

    case class OperationSequence(head: Ast.Node, tail: List[(Operator.Value, Ast.Node)])

    val scope = parentFunction.scope

    def findSignature(descriptor: String): FunctionDef.Signature =
      scope.getOrElse(descriptor,
        throw new CalculatorParserException(
          s"Error in body of function $parentFunction.\n" +
            s"Cannot find function $descriptor."))

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

      val signature = findSignature(descriptor)

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

            val result = Ast.NumberNode(negative = false, node.number)

            WrapperNode(result, node.leadingSpace) :: process(tail)
          }

          case (node: ParserAst.OperationNode) :: tail => {

            val result = OperationNode(node.operator)

            result :: process(tail)
          }

          case Nil =>
            Nil
        }

      transformOperation(process(nodes))
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

        operator.map(x => applyArgumentsToOperator(x, List(result))).getOrElse(result)
      }

      def handleBlockNode(parameter: FunctionDef.Signature,
                          position: Int,
                          node: ParserAst.BlockNode,
                          operator: Option[String]): Ast.Node = {

        val result = getInnerNode(node)

        verifySignature(signature.descriptor, parameter, result.signature, position)

        operator.map(x => applyArgumentsToOperator(x, List(result))).getOrElse(result)
      }

      def handleNumberNode(parameter: FunctionDef.Signature,
                           position: Int,
                           node: ParserAst.NumberNode,
                           operator: Option[String]): Ast.Node = {

        val result = Ast.NumberNode(operator.contains(Operator.Neg), node.number)

        verifySignature(signature.descriptor, parameter, result.signature, position)

        result
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

          case ParserAst.OperationNode(Operator.Add, true) :: (node: ParserAst.FunctionNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleFunctionNode(parameter, position, node, Some(Operator.Pos)))

          case ParserAst.OperationNode(Operator.Sub, true) :: (node: ParserAst.FunctionNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleFunctionNode(parameter, position, node, Some(Operator.Neg)))

          case (node: ParserAst.BlockNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleBlockNode(parameter, position, node, None))

          case ParserAst.OperationNode(Operator.Add, true) :: (node: ParserAst.BlockNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleBlockNode(parameter, position, node, Some(Operator.Pos)))

          case ParserAst.OperationNode(Operator.Sub, true) :: (node: ParserAst.BlockNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleBlockNode(parameter, position, node, Some(Operator.Neg)))

          case (node: ParserAst.NumberNode) :: tail
            if (node.leadingSpace) =>
            (tail, handleNumberNode(parameter, position, node, None))

          case ParserAst.OperationNode(Operator.Add, true) :: (node: ParserAst.NumberNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleNumberNode(parameter, position, node, Some(Operator.Pos)))

          case ParserAst.OperationNode(Operator.Sub, true) :: (node: ParserAst.NumberNode) :: tail
            if (!node.leadingSpace) =>
            (tail, handleNumberNode(parameter, position, node, Some(Operator.Neg)))

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

    def transformOperation(nodes: List[Node]): Ast.Node = {

      def flattenOperations(nodes: OperationSequence,
                            operators: List[Operator.Value]): OperationSequence =
        nodes.tail match {

          case (operator, next) :: tail if (operators.contains(operator)) => {

            val node = applyArgumentsToOperator(operator.toString, List(nodes.head, next))

            flattenOperations(OperationSequence(node, tail), operators)
          }

          case (operator, next) :: tail => {

            val list = flattenOperations(OperationSequence(next, tail), operators)

            OperationSequence(nodes.head, (operator, list.head) :: list.tail)
          }

          case Nil =>
            OperationSequence(nodes.head, Nil)
        }

      def transformOperationSequence(nodes: OperationSequence): Ast.Node = {

        val result =
          flattenOperations(
            flattenOperations(
              flattenOperations(
                flattenOperations(nodes,
                  List(Operator.Pow)),
                List(Operator.Mul, Operator.Div, Operator.Mod)),
              List(Operator.Add, Operator.Sub)),
            List(Operator.Eq, Operator.Ne, Operator.Gt, Operator.Ge, Operator.Lt, Operator.Le))

        if (result.tail.nonEmpty)
          throw new IllegalStateException("Something went wrong while creating AST.")

        result.head
      }

      def toggleNumberSign(node: Ast.NumberNode, negative: Boolean): Ast.Node = {

        Ast.NumberNode((node.negative && !negative) || (!node.negative && negative), node.number)
      }

      def transformOperand(nodes: List[Node]): (List[Node], Ast.Node) =
        nodes match {

          case OperationNode(Operator.Add) :: WrapperNode(node: Ast.FunctionNode, false) :: tail =>
            (tail, applyArgumentsToOperator(Operator.Pos, List(node)))

          case OperationNode(Operator.Sub) :: WrapperNode(node: Ast.FunctionNode, false) :: tail =>
            (tail, applyArgumentsToOperator(Operator.Neg, List(node)))

          case OperationNode(Operator.Add) :: WrapperNode(node: Ast.ApplicationNode, false) :: tail =>
            (tail, applyArgumentsToOperator(Operator.Pos, List(node)))

          case OperationNode(Operator.Sub) :: WrapperNode(node: Ast.ApplicationNode, false) :: tail =>
            (tail, applyArgumentsToOperator(Operator.Neg, List(node)))

          case OperationNode(Operator.Add) :: WrapperNode(node: Ast.NumberNode, false) :: tail =>
            (tail, toggleNumberSign(node, negative = false))

          case OperationNode(Operator.Sub) :: WrapperNode(node: Ast.NumberNode, false) :: tail =>
            (tail, toggleNumberSign(node, negative = true))

          case WrapperNode(node: Ast.Node, _) :: tail =>
            (tail, node)

          case Nil =>
            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Syntax error caused by a missing operand.")

          case _ =>
            throw new CalculatorParserException(
              s"Error in body of function $parentFunction.\n" +
                s"Syntax error caused by incorrect usage of operators.")
        }

      def process(nodes: List[Node]): Ast.Node = {

        def transformRest(nodes: List[Node]): List[(Operator.Value, Ast.Node)] =
          nodes match {

            case OperationNode(operator) :: tail => {

              val (rest, node) = transformOperand(tail)

              (operator, node) :: transformRest(rest)
            }

            case Nil =>
              Nil

            case _ =>
              throw new CalculatorParserException(
                s"Error in body of function $parentFunction.\n" +
                  s"Syntax error caused by a unbound functions.")
          }

        val (rest, head) = transformOperand(nodes)

        transformOperationSequence(OperationSequence(head, transformRest(rest)))
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

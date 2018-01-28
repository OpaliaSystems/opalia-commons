package systems.opalia.commons.scripting.calculator

import scala.util.Try
import systems.opalia.commons.utility.RegexParsersEx


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

  private object Parser
    extends RegexParsersEx {

    override def skipWhitespace = false

    def `descriptor`: Parser[String] =
      """((\p{InGreek})|[a-zA-Z])((\p{InGreek})|[a-zA-Z0-9]|_)*""".r ^^ (_.toString)

    def `declaration`: Parser[ParserAst.Declaration] =
      """\s*""".r ~> (
        `descriptor` ~
          rep("""\s+""".r ~> `signature-part`) ~ ("""\s+\\\s+""".r ~> `signature-part`).? ~
          ("""\s+\:\s""".r ~> `body`)) <~
        """\s*""".r ^^ {
        case descriptor ~ parameters ~ target ~ body =>

          ParserAst.Declaration(FunctionDef.Signature(descriptor, parameters, target), body)
      }

    def `signature-part`: Parser[FunctionDef.Signature] =
      `signature-part-0` | `signature-part-N`

    def `signature-part-0`: Parser[FunctionDef.Signature] =
      `descriptor` ^^ {
        descriptor =>

          FunctionDef.Signature(descriptor, Nil, None)
      }

    def `signature-part-N`: Parser[FunctionDef.Signature] =
      """\(\s*""".r ~> (
        `descriptor` ~
          rep("""\s+""".r ~> `signature-part`) ~ ("""\s+\\\s+""".r ~> `signature-part`).?) <~
        """\s*\)""".r ^^ {
        case descriptor ~ parameters ~ target =>

          FunctionDef.Signature(descriptor, parameters, target)
      }

    def `body`: Parser[ParserAst.Body] =
      `node` ~ rep(`node`) ^^ {
        case head ~ tail =>

          ParserAst.Body(head :: tail)
      }

    def `node`: Parser[ParserAst.Node] =
      `lambda-node` | `block-node` | `function-node` | `number-node` | `operation-node`

    def `lambda-node`: Parser[ParserAst.LambdaFunctionNode] =
      """\s*""".r ~ ("""\(\s*""".r ~> (
        `signature-part` ~ rep("""\s+""".r ~> `signature-part`) ~ ("""\s+\\\s+""".r ~> `signature-part`).? ~
          ("""\s+\.\s""".r ~> `body`)) <~
        """\s*\)""".r) ^^ {
        case space ~ (head ~ tail ~ target ~ body) =>

          ParserAst.LambdaFunctionNode(FunctionDef.Signature("", head :: tail, target), body, space.nonEmpty)
      }

    def `function-node`: Parser[ParserAst.FunctionNode] =
      """\s*""".r ~ `descriptor` ^^ {
        case space ~ descriptor =>

          ParserAst.FunctionNode(descriptor, space.nonEmpty)
      }

    def `block-node`: Parser[ParserAst.BlockNode] =
      """\s*""".r ~ ("""\(""".r ~> (`node` ~ rep(`node`)) <~ """\s*\)""".r) ^^ {
        case space ~ (head ~ tail) =>

          ParserAst.BlockNode(head :: tail, space.nonEmpty)
      }

    def `number-node`: Parser[ParserAst.NumberNode] =
      """\s*""".r ~ """(\d+)(\.\d+)?([eE][+-]?\d+)?""".r ^^ {
        case space ~ value =>

          ParserAst.NumberNode(value, space.nonEmpty)
      }

    def `operation-node`: Parser[ParserAst.Node] =
      """\s*""".r ~ `operator` ^^ {
        case space ~ operator =>

          ParserAst.OperationNode(operator, space.nonEmpty)
      }

    def `operator`: Parser[Operator.Value] =
      "+" ^^ (_ => Operator.Add) |
        "-" ^^ (_ => Operator.Sub) |
        "*" ^^ (_ => Operator.Mul) |
        "×" ^^ (_ => Operator.Mul) |
        "/" ^^ (_ => Operator.Div) |
        "÷" ^^ (_ => Operator.Div) |
        "%" ^^ (_ => Operator.Mod) |
        "^" ^^ (_ => Operator.Pow) |
        "<=" ^^ (_ => Operator.Le) |
        "≤" ^^ (_ => Operator.Le) |
        ">=" ^^ (_ => Operator.Ge) |
        "≥" ^^ (_ => Operator.Ge) |
        "<>" ^^ (_ => Operator.Ne) |
        "≠" ^^ (_ => Operator.Ne) |
        "=" ^^ (_ => Operator.Eq) |
        "<" ^^ (_ => Operator.Lt) |
        ">" ^^ (_ => Operator.Gt)

    def parseDeclaration(value: String): ParserAst.Declaration =
      parseAll(`declaration`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new CalculatorParserException(s"Failed to parse function declaration.\n${failure.msg}")
      }

    def parseBody(value: String): ParserAst.Body =
      parseAll(`body`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new CalculatorParserException(s"Failed to parse function body.\n${failure.msg}")
      }

    def parseDescriptor(value: String): String =
      parseAll(`descriptor`, value) match {
        case Success(result, _) =>
          result
        case failure: NoSuccess =>
          throw new CalculatorParserException(s"Failed to parse function descriptor.\n${failure.msg}")
      }
  }

  def validateDescriptor(descriptor: String): Boolean = {

    Try(Parser.parseDescriptor(descriptor)).isSuccess
  }

  def parse(string: String,
            handleFunction: (FunctionDef) => Unit,
            getGlobalFunctions: () => Set[FunctionDef]): List[Ast.Body] = {

    def group(lines: List[String], acc: String = ""): List[String] =
      lines match {
        case line :: rest if (line.contains(":")) =>
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
        .map(Parser.parseDeclaration)
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

    val body = Parser.parseBody(lines.mkString(" ").trim)

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

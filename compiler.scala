package scalox

import java.io.BufferedReader
import java.io.InputStreamReader
import scalox.{TokenType, Token, LoxRuntimeLiteral, Scanner}

object Interpreter:
  var hadRuntimeError = false

  case class LoxRuntimeError(token: Token, msg: String)
      extends RuntimeException(msg)

  def interpret(expr: Expr): Unit =
    try {
      println(evaluate(expr))
    } catch {
      case e: LoxRuntimeError =>
        runtimeError(e)
    }

  def runtimeError(error: LoxRuntimeError): Unit =
    Console.err.println(
      s"${error.token.line}: ${error.token.lexeme} - ${error.msg}"
    )
    hadRuntimeError = true

  private def double(value: LoxRuntimeLiteral): Double =
    value.asInstanceOf[Double]

  private def checkNumOperand(op: Token, operand: LoxRuntimeLiteral): Unit =
    if operand.isInstanceOf[Double] then ()
    else throw LoxRuntimeError(op, "Operand must be a number.")

  private def checkNumOperands(
      op: Token,
      operandLeft: LoxRuntimeLiteral,
      operandRight: LoxRuntimeLiteral
  ): Unit =
    if operandLeft.isInstanceOf[Double] && operandRight.isInstanceOf[Double]
    then ()
    else throw LoxRuntimeError(op, "Operand must be a number.")

  def evaluate(expr: Expr): LoxRuntimeLiteral =
    expr match {
      case Binary(left, operator, right) =>
        val evaluatedLeft = evaluate(left)
        val evaluatedRight = evaluate(right)

        operator.tokenType match {
          case TokenType.Plus =>
            (evaluatedLeft, evaluatedRight) match {
              case (l: String, r: String) => l + r.toString()
              case (l: Double, r: Double) => l + r
              case _ =>
                throw LoxRuntimeError(
                  operator,
                  s"Invalid operand type. Both '$evaluatedLeft' and '$evaluatedRight' must be a number or string"
                )
            }
          case TokenType.Minus =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) - double(evaluatedRight)
          case TokenType.Star =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) * double(evaluatedRight)
          case TokenType.Slash =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) / double(evaluatedRight)
          case TokenType.BangEqual =>
            evaluatedLeft != evaluatedRight
          case TokenType.EqualEqual =>
            evaluatedLeft == evaluatedRight
          case TokenType.Greater =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) > double(evaluatedRight)
          case TokenType.GreaterEqual =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) >= double(evaluatedRight)
          case TokenType.Less =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) < double(evaluatedRight)
          case TokenType.LessEqual =>
            checkNumOperands(operator, evaluatedLeft, evaluatedRight)
            double(evaluatedLeft) <= double(evaluatedRight)
          case _ => throw new RuntimeException(s"Invalid operator $operator")
        }

      case Grouping(expression) => evaluate(expression)
      case Literal(value)       => value
      case Unary(operator, right) =>
        val evaluatedRight = evaluate(right)

        operator.tokenType match {
          case TokenType.Minus => -evaluatedRight.asInstanceOf[Double]
          case TokenType.Bang  => !isTruthy(evaluatedRight)
          case _ => throw new RuntimeException(s"Invalid operator $operator")
        }
    }

  def isTruthy(value: LoxRuntimeLiteral): Boolean =
    value match {
      case null  => false
      case false => false
      case _     => true
    }

object Compiler:
  var hadError = false

  def error(line: Int, msg: String): Unit =
    report(line, "", msg)

  def error(token: Token, msg: String): Unit =
    if token.tokenType == TokenType.EOF
    then report(token.line, "at end", msg)
    else report(token.line, s"at '${token.lexeme}'", msg)

  def report(line: Int, where: String, msg: String): Unit =
    System.err.println(s"[line $line] Error $where: $msg")

  def runFile(filePath: String) =
    val code = io.Source.fromFile(filePath, "utf-8").getLines().mkString
    run(code)
    if hadError
    then System.exit(65)

    if Interpreter.hadRuntimeError
    then System.exit(70)

  def runPrompt() =
    val reader = new BufferedReader(new InputStreamReader(System.in))

    var continue = true
    while (continue) {
      print("> ")
      val line = reader.readLine()
      if line == null then continue = false
      else run(line)
      hadError = false
    }

  def run(code: String) =
    val scanner = new Scanner(code)
    val tokens = scanner.scanTokens()
    val parser = new Parser(tokens.toArray)
    parser.parse() match {
      case Some(expr) =>
        println(AstPrinter.print(expr))
        Interpreter.interpret(expr)
      case None =>
    }

    // if hadError
    // then return

    // tokens.foreach(println)

  def main(args: Array[String]): Unit =
    if args.length > 1 then
      println("Usage scalox [script]")
      System.exit(64)
    else if args.length == 1 then runFile(args(0))
    else runPrompt()

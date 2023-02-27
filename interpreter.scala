package scalox

class Environment:
  val values = collection.mutable.Map[String, LoxRuntimeLiteral]()

  def define(name: String, value: LoxRuntimeLiteral): Unit =
    values += name -> value

  def get(name: Token): LoxRuntimeLiteral =
    values.get(name.lexeme) match {
      case Some(value) => value
      case None =>
        throw LoxRuntimeError(name, "Undefined variable.")
    }

case class LoxRuntimeError(token: Token, msg: String)
    extends RuntimeException(msg)

class Interpreter:
  var hadRuntimeError = false
  val env = new Environment()

  def interpret(expr: Expr): Unit =
    try {
      println(evaluate(expr))
    } catch {
      case e: LoxRuntimeError =>
        runtimeError(e)
    }

  def interpret(stmts: List[Stmt]): Unit =
    try {
      stmts.foreach {
        case Expression(expr) => evaluate(expr)
        case Print(expr)      => println(evaluate(expr))
        case Var(name, init) =>
          env.define(name.lexeme, evaluate(init))
      }
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
      case Variable(name) => env.get(name)
    }

  def isTruthy(value: LoxRuntimeLiteral): Boolean =
    value match {
      case null  => false
      case false => false
      case _     => true
    }

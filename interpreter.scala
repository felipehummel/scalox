package scalox

class Environment:
  val values = collection.mutable.Map[String, LoxRuntimeLiteral]()
  var enclosing: Option[Environment] = None

  def define(name: String, value: LoxRuntimeLiteral): Unit =
    values += name -> value

  def assign(name: Token, value: LoxRuntimeLiteral): Unit =
    enclosing match {
      case Some(enclosedEnv) => enclosedEnv.assign(name, value)
      case None =>
        values.get(name.lexeme) match {
          case Some(_) =>
            values += name.lexeme -> value
          case None =>
            throw LoxRuntimeError(name, "Undefined variable.")
        }
    }

  def isDefined(name: Token): Boolean =
    enclosing.map(_.isDefined(name)).getOrElse(values.contains(name.lexeme))

  def get(name: Token): LoxRuntimeLiteral =
    values.get(name.lexeme).getOrElse {
      enclosing.map(_.get(name)).getOrElse {
        throw LoxRuntimeError(name, "Undefined variable.")
      }
    }

case class LoxRuntimeError(token: Token, msg: String)
    extends RuntimeException(msg)

class Interpreter:
  var hadRuntimeError = false
  var env = new Environment()

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
        case Block(stmts) =>
          executeBlock(stmts)
      }
    } catch {
      case e: LoxRuntimeError =>
        runtimeError(e)
    }

  def executeBlock(stmts: List[Stmt]): Unit =
    val newBlockEnv = new Environment()
    newBlockEnv.enclosing = Some(env)

    val previousEnv = env
    try {
      env = newBlockEnv
      interpret(stmts)
    } finally {
      env = previousEnv
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
      case Variable(name) =>
        env.get(name)
      case Assignment(name, value) =>
        val evaluated = evaluate(value)
        env.assign(name, evaluated)
        evaluated
    }

  def isTruthy(value: LoxRuntimeLiteral): Boolean =
    value match {
      case null  => false
      case false => false
      case _     => true
    }

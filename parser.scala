package scalox

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: LoxRuntimeLiteral) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr

object AstPrinter:
  private def parenthesize(name: String, exprs: Expr*): String =
    val builder = new StringBuilder()
    builder.append("(").append(name)
    exprs.foreach { expr =>
      builder.append(" ")
      builder.append(print(expr))
    }
    builder.append(")")
    builder.toString()

  def print(expr: Expr): String =
    expr match {
      case Binary(left, operator, right) =>
        parenthesize(operator.lexeme, left, right)
      case Grouping(expression)   => parenthesize("group", expression)
      case Literal(value)         => value.toString
      case Unary(operator, right) => parenthesize(operator.lexeme, right)
    }

class ParserError extends RuntimeException

object Parser:
  def parse(code: String): Expr =
    val scanner = Scanner(code)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens.toArray)
    parser.expression()

class Parser(tokens: Array[Token]):
  private var current = 0

  def parse(): Option[Expr] =
    try {
      Some(expression())
    } catch {
      case e: ParserError => None
    }

  def expression(): Expr =
    val expr = equality()
    expr

  def equality(): Expr =
    var expr = comparison()

    while (matchNext(TokenType.BangEqual, TokenType.EqualEqual)) {
      val operator = previous()
      val right = comparison()
      expr = Binary(expr, operator, right)
    }

    expr

  def comparison(): Expr =
    var expr = term()

    while (
      matchNext(
        TokenType.Greater,
        TokenType.GreaterEqual,
        TokenType.Less,
        TokenType.LessEqual
      )
    ) {
      val operator = previous()
      val right = term()
      expr = Binary(expr, operator, right)
    }

    expr

  def term(): Expr =
    var expr = factor()

    while (matchNext(TokenType.Minus, TokenType.Plus)) {
      val operator = previous()
      val right = factor()
      expr = Binary(expr, operator, right)
    }

    expr

  def factor(): Expr =
    var expr = unary()

    while (matchNext(TokenType.Slash, TokenType.Star)) {
      val operator = previous()
      val right = unary()
      expr = Binary(expr, operator, right)
    }

    expr

  def unary(): Expr =
    if (matchNext(TokenType.Bang, TokenType.Minus)) {
      val operator = previous()
      val right = unary()
      Unary(operator, right)
    } else primary()

  def primary(): Expr =
    if (matchNext(TokenType.False)) Literal(false)
    else if (matchNext(TokenType.True)) Literal(true)
    else if (matchNext(TokenType.Nil)) Literal(null)
    else if (matchNext(TokenType.Number, TokenType.String))
      Literal(previous().literal.get)
    else if (matchNext(TokenType.LeftParen)) {
      val expr = expression()
      consume(TokenType.RightParen, "Expect ')' after expression.")
      Grouping(expr)
    } else
      throw error(
        peek(),
        s"Unexpected token ${peek().tokenType} at line ${peek().line}"
      )

  def consume(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) advance()
    else throw new RuntimeException(message)

  def synchronize(): Unit =
    advance()

    while (!isAtEnd()) {
      if (previous().tokenType == TokenType.SemiColon) return

      peek().tokenType match {
        case TokenType.Class | TokenType.Fun | TokenType.Var | TokenType.For |
            TokenType.If | TokenType.While | TokenType.Print |
            TokenType.Return =>
          return
        case _ => advance()
      }
    }

  def error(token: Token, message: String): ParserError =
    Compiler.error(token, message)
    new ParserError()

  def matchNext(tokenTypes: TokenType*): Boolean =
    if (tokenTypes.find(t => check(t)).isDefined) {
      advance()
      true
    } else false

  def check(tokenType: TokenType): Boolean =
    if (isAtEnd()) false
    else peek().tokenType == tokenType

  def advance(): Token =
    if (!isAtEnd()) current += 1
    previous()

  def isAtEnd(): Boolean =
    peek().tokenType == TokenType.EOF

  def peek(): Token =
    tokens(current)

  def previous(): Token =
    tokens(current - 1)

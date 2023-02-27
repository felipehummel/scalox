package scalox

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: LoxRuntimeLiteral) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Variable(name: Token) extends Expr
case class Assignment(name: Token, value: Expr) extends Expr

sealed trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(name: Token, initializer: Expr) extends Stmt
case class Block(statements: List[Stmt]) extends Stmt

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
      case Variable(name)         => parenthesize("var", Literal(name.lexeme))
      case Assignment(name, value) =>
        parenthesize("=", Literal(name.lexeme), value)
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

  def parse(): List[Stmt] =
    try {
      var statements = List[Stmt]()
      while (!isAtEnd()) {
        statements = statements :+ declaration()
      }

      statements
    } catch {
      case e: ParserError => Nil
    }

  def declaration(): Stmt =
    if (matchNext(TokenType.Var)) varDeclaration()
    else statement()

  def varDeclaration(): Stmt =
    val name = consume(TokenType.Identifier, "Expect variable name.")
    consume(
      TokenType.Equal,
      "Expect '=' after variable name in variable declation."
    )
    var initializer = expression()

    consume(TokenType.SemiColon, "Expect ';' after variable declaration.")
    Var(name, initializer)

  def statement(): Stmt =
    if (matchNext(TokenType.Print)) printStatement()
    else if (matchNext(TokenType.LeftBrace)) Block(block())
    else expressionStatement()

  def block(): List[Stmt] =
    var statements = List[Stmt]()
    while (!check(TokenType.RightBrace) && !isAtEnd()) {
      statements = statements :+ declaration()
    }

    consume(TokenType.RightBrace, "Expect '}' after block.")
    List(Block(statements))

  def printStatement(): Stmt =
    val value = expression()
    consume(TokenType.SemiColon, "Expect ';' after value.")
    Print(value)

  def expressionStatement(): Stmt =
    val expr = expression()
    consume(TokenType.SemiColon, "Expect ';' after expression.")
    Expression(expr)

  def expression(): Expr =
    assignment()

  def assignment(): Expr =
    var expr = equality()

    if (matchNext(TokenType.Equal)) {
      val equals = previous()
      val value = assignment()

      expr match {
        case Variable(name) => Assignment(name, value)
        case _              => throw error(equals, "Invalid assignment target")
      }
    } else expr

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
    } else if (matchNext(TokenType.Identifier)) {
      Variable(previous())
    } else
      throw error(
        peek(),
        s"Unexpected token ${peek().tokenType} at line ${peek().line}"
      )

  def consume(tokenType: TokenType, message: String): Token =
    if (check(tokenType)) advance()
    else throw error(peek(), message)

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

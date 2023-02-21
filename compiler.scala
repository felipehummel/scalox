import java.io.BufferedReader
import java.io.InputStreamReader

enum TokenType {
    case LeftParen
    case RightParen
    case LeftBrace
    case RightBrace
    case Comma
    case Dot
    case Minus
    case Plus
    case SemiColon
    case Slash
    case Star

    case Bang
    case BangEqual
    case Equal
    case EqualEqual
    case Greater
    case GreaterEqual
    case Less
    case LessEqual

    case Identifier
    case String
    case Number

    case And
    case Class
    case Else
    case False
    case Fun
    case For
    case If
    case Nil
    case Or
    case Print
    case Return
    case Super
    case This
    case True
    case Var
    case While

    case EOF
}

type LoxRuntimeLiteral = Symbol | String | Double

case class Token(tokenType: TokenType, lexeme: String, literal: Option[LoxRuntimeLiteral], line: Int)

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: LoxRuntimeLiteral) extends Expr

class Scanner(code: String):
    val keywordLookup = TokenType.values.map( v => v.toString.toLowerCase -> v).toMap

    def scanTokens(): List[Token] =
        import TokenType._
        var tokens  = List[Token]()
        var start = 0
        var current = 0
        var line = 1
        
        def newToken(tokenType: TokenType, literal: Option[LoxRuntimeLiteral] = None): Option[Token] =
            val text = code.substring(start, current)
            Some(Token(tokenType, text, literal, line))

        def isAtEnd(): Boolean =
            current >= code.length
        
        def matchNext(expected: Char): Boolean =
            if isAtEnd() || code.charAt(current) != expected then
                false
            else
                current += 1
                true
        
        def peek(): Char = 
            if isAtEnd() then
                '\u0000'
            else
                code.charAt(current)
        
        def consumeUntilEndOfLine(): Unit =
            while (!isAtEnd() && peek() != '\n') {
                advance()
            }
        
        def advance(): Char =
            current += 1
            code.charAt(current - 1)

        def peekNext(): Char =
            if current + 1 >= code.length then
                '\u0000'
            else
                code.charAt(current + 1)
        
        def scanString(): Option[Token] =
            while (peek() != '"' && !isAtEnd()) {
                if peek() == '\n' then line += 1
                advance()
            }

            if isAtEnd() then
                Compiler.error(line, "Unterminated string.")
                return None

            advance()

            val value = code.substring(start + 1, current - 1)
            newToken(TokenType.String, Some(value))
        
        def scanNumber(): Option[Token] =
            while isDigit(peek()) do
                advance()

            if peek() == '.' && isDigit(peekNext()) then {
                advance()

                while isDigit(peek()) do
                    advance()
            }

            val value = code.substring(start, current)
            val doubleVal = java.lang.Double.parseDouble(value)
            newToken(TokenType.Number, Some(doubleVal))
        
        def scanIdentifier(): Option[Token] =
            while isAlphaNumeric(peek()) do
                advance()

            val text = code.substring(start, current)
            keywordLookup.get(text).map(newToken(_)).getOrElse(newToken(TokenType.Identifier))
        
        def isDigit(c: Char): Boolean =
            c >= '0' && c <= '9'
        
        def isAlpha(c: Char): Boolean =
            c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c == '_'

        def isAlphaNumeric(c: Char): Boolean =
            isAlpha(c) || isDigit(c)
         
        def scanToken(): Option[Token] =
            val c = advance()
            c match {
                case '(' => newToken(LeftParen)
                case ')' => newToken(RightParen)
                case '{' => newToken(LeftBrace)
                case '}' => newToken(RightBrace)
                case ',' => newToken(Comma)
                case '.' => newToken(Dot)
                case '-' => newToken(Minus)
                case '+' => newToken(Plus)
                case ';' => newToken(SemiColon)
                case '*' => newToken(Star)
                case '!' => if matchNext('=') then newToken(BangEqual) else newToken(Bang)
                case '=' => if matchNext('=') then newToken(EqualEqual) else newToken(Equal)
                case '<' => if matchNext('=') then newToken(LessEqual) else newToken(Less)
                case '>' => if matchNext('=') then newToken(GreaterEqual) else newToken(Greater)
                case '/' => if matchNext('/') then { consumeUntilEndOfLine(); None } else newToken(Slash)
                case ' ' | '\r' | '\t' => None
                case '\n'           => line += 1; None
                case '"'            =>  scanString()
                case _ => 
                    if isDigit(c)
                    then 
                        scanNumber()
                    else
                        if isAlpha(c)
                        then
                            scanIdentifier()
                        else
                            Compiler.error(line, "Unexpected character"); None
            }

        while(!isAtEnd()) {
            start = current
            scanToken().foreach { token =>
                tokens = tokens :+ token
            }
        }

        tokens = tokens :+ Token(TokenType.EOF, "", null, line)
        tokens
        

        
object Compiler:
    var hadError = false

    def error(line: Int, msg: String): Unit = 
        report(line, "", msg)

    def report(line: Int, where: String, msg: String): Unit =
        System.err.println(s"[line $line] Error $where: $msg")

    def runFile(filePath: String) =
        val code = io.Source.fromFile(filePath, "utf-8").getLines().mkString
        run(code)
        if hadError
        then System.exit(65)


    def runPrompt() =
        val reader = new BufferedReader(new InputStreamReader(System.in))

        var continue = true
        while(continue) {
            print("> ")
            val line = reader.readLine()
            if line == null then
                continue = false 
            else
                run(line)
            hadError = false
        }

    def run(code: String) =
        val scanner = new Scanner(code)
        val tokens = scanner.scanTokens()

        tokens.foreach(println)

    def main(args: Array[String]): Unit =
        if args.length > 1 then
            println("Usage scalox [script]")
            System.exit(64)
        else
            if args.length == 1 then
                runFile(args(0))
            else
                runPrompt()
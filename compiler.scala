package scalox

import java.io.BufferedReader
import java.io.InputStreamReader
import scalox.{TokenType, Token, LoxRuntimeLiteral, Scanner, Interpreter}

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
    val interpreter = new Interpreter()

    val code = io.Source.fromFile(filePath, "utf-8").getLines().mkString
    run(code, interpreter)
    if hadError
    then System.exit(65)

    if interpreter.hadRuntimeError
    then System.exit(70)

  def runPrompt() =
    val interpreter = new Interpreter()
    val reader = new BufferedReader(new InputStreamReader(System.in))

    var continue = true
    while (continue) {
      print("> ")
      val line = reader.readLine()
      if line == null then continue = false
      else run(line, interpreter)
      hadError = false
    }

  def run(code: String, interpreter: Interpreter) =
    val scanner = new Scanner(code)
    val tokens = scanner.scanTokens()
    val parser = new Parser(tokens.toArray)
    val stmts = parser.parse()
    interpreter.interpret(stmts)

    // if hadError
    // then return

    // tokens.foreach(println)

  def main(args: Array[String]): Unit =
    if args.length > 1 then
      println("Usage scalox [script]")
      System.exit(64)
    else if args.length == 1 then runFile(args(0))
    else runPrompt()

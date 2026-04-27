package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
import parsley.expr.{precedence, Ops, InfixL}
import parsley.combinator.{sepBy, some, option, many}
import parsley.Parsley.atomic

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, double, identifier, fully, sumKeyword, prodKeyword, funcKeyword, returnKeyword}
import DSL.frontend.AST._

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[String, Program] = {
    this.parser.parse(input) match {
      case Success(p)   => Success(p)
      case Failure(msg) => Failure(msg.toString)
    }
  }

  private def wrapInSumIfNeeded(e: Expr): Expr = e match {
    case _: Sum | _: Prod => e
    case _                => Sum(e)
  }

  private lazy val parser: Parsley[Program] = fully(program)

  private lazy val program: Parsley[Program] =
    some(stmt <~ option(";")).map(Program.apply)

  private lazy val stmt: Parsley[Stmt] =
    assignStmt <|> returnStmt <|> funcDecl <|> ifStmt <|> exprStmt

  private lazy val assignStmt: Parsley[Assign] =
    (atomic(identifier <~ "=") <~> expr).map { case (id, e) => Assign(id, e) }

  private lazy val returnStmt: Parsley[Return] =
    (returnKeyword ~> expr).map(Return.apply)

  private lazy val funcDecl: Parsley[Func] =
    atomic(funcKeyword ~> identifier <~ "(").flatMap { name =>
      (sepBy(identifier, ",") <~ ")" <~> block).map { case (params, body) =>
        Func(name, params, body)
      }
    }

  /** logic for { stmt; stmt } blocks used in funcs and ifs */
  private lazy val block: Parsley[List[Stmt]] = 
    "{" ~> some(stmt <~ option(";")) <~ "}"

  private lazy val ifStmt: Parsley[If] = {
    val ifBranch = ("if" ~> expr) <~> block
    val elifBranch = ("elif" ~> expr) <~> block
    val elsePart = "else" ~> block
    
    (ifBranch <~> many(elifBranch) <~> option(elsePart)).map {
      case ((firstBranch, elifs), maybeElse) => 
        If(firstBranch :: elifs, maybeElse)
    }
  }

  private lazy val exprStmt: Parsley[ExprStmt] =
    expr.map(e => ExprStmt(wrapInSumIfNeeded(e)))

  /** Expressions */
  
  private val diceOp = char('d')

  private lazy val expr: Parsley[Expr] = precedence[Expr](term)(
    // Comparison layer
    Ops(InfixL)("===" #> IdenEq.apply, "==" #> Eq.apply),
    // Addition layer
    Ops(InfixL)("+" #> Add.apply, "-" #> Sub.apply)
  )

  private lazy val term: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(diceOp #> Dice.apply),
    Ops(InfixL)("*" #> Mul.apply, "/" #> Div.apply)
  )

  /** Atoms */

  private lazy val atom: Parsley[Expr] = 
    literal <|> customDistLiteral <|> atomic(sumCall) <|> atomic(prodCall) <|> atomic(prefixDice) <|> funcCall <|> identRef <|> parens

  private lazy val literal: Parsley[IntLiteral] = 
    integer.map(IntLiteral.apply)
  
  private lazy val funcCall: Parsley[Call] =
    (atomic(identifier <~ "(") <~> sepBy(expr, ",") <~ ")").map { case (name, args) => Call(name, args) }

  private lazy val identRef: Parsley[Ident] =
    identifier.map(Ident.apply)

  private lazy val customDistLiteral: Parsley[CustomDist] = {
    val entry = integer <~> (":" ~> double)
    ("{" ~> sepBy(entry, ",") <~ "}").map { entries =>
      CustomDist(entries.toMap)
    }
  }

  private lazy val sumCall: Parsley[Sum] = 
    (sumKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Sum.apply)

  private lazy val prodCall: Parsley[Prod] = 
    (prodKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Prod.apply)

  private lazy val prefixDice: Parsley[Dice] = 
    diceOp ~> atom.map(sides => Dice(IntLiteral(1), sides))

  private lazy val parens: Parsley[Expr] = "(" ~> expr <~ ")"
}
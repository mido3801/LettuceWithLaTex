package project3

import scala.util.parsing.combinator.RegexParsers

class LettuceParser extends RegexParsers {
  def floatingPointNumber: Parser[String] = {
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
  }

  def identifier: Parser[String] = {
    """[a-zA-Z_][a-zA-Z0-9_]*""".r
  }
  def LetKwd: Parser[String] = {
    "let"  | "Let"
  }

  def compOperator: Parser[String] = {
    ">=" | "<=" | ">" | "<" | "==" | "!="
  }

  def funDefinition: Parser[FunDef] = {
    ("function" ~"(") ~> (identifier) ~ (")" ~> exprLev1)  ^^ {
      case id~e => FunDef(id, e)
    }
  }

  def sinFunc: Parser[Expr] = {
    "sin(" ~> exprLev1 <~ ")" ^^{
      case e => Sin(e)
    }
  }

  def cosFunc: Parser[Expr] = {
    ("cos" ~ "(") ~> exprLev1 <~ ")" ^^{
      case e => Cos(e)
    }
  }

  def expFunc: Parser[Expr] = {
    ("exp" ~ "(") ~> exprLev1 <~ ")" ^^{
      case e => Exp(e)
    }
  }

  def formulaKeyword: Parser[String]={
    "formula" | "Formula"
  }

  def funCallArgs: Parser[Expr] = {
    "(" ~> exprLev1 <~ ")"
  }

  def exprLev1: Parser[Expr] = {
    val opt1 = ("let" ~> identifier) ~ ("=" ~> exprLev1) ~ ("in" ~> exprLev1)  ^^ {
      case s1 ~ e1 ~ e2 => Let(s1, e1, e2)
    }
    val opt2 = ("letrec" ~> identifier) ~ ("=" ~> funDefinition) ~ ("in" ~> exprLev1 ) ^^ {
      case s1 ~ (FunDef(id, e1)) ~ e2 => {
        LetRec(s1, id, e1,  e2)
      }
    }
    val opt3 = funDefinition ^^ { s => s }

    val opt4 = ("if" ~> exprLev2)~("then" ~> exprLev1)~("else" ~> exprLev1) ^^ {
      case e ~ e1 ~ e2 => IfThenElse(e, e1, e2)
    }

    val opt5 = "formula("~>exprLev1<~")"^^{
      case e => Latex(e)
    }

    val opt6 = "symbol("~>identifier<~")"^^{
      case e => Symbol(e)
    }
    val opt7 = "sin("~>exprLev1<~")"^^{
      case e => Sin(e)
    }
    val opt8 = "cos("~>exprLev1<~")"^^{
      case e => Cos(e)
    }
    val opt9 = "exp("~>exprLev1<~")"^^{
      case e => Exp(e)
    }

    val opt10 = "derivative("~>exprLev1<~")"^^{
      case e => Deriv(e,Symbol("x"))
    }

    val opt11 = "num("~>floatingPointNumber<~")"^^{
      case e => SymNum(e)
    }

    val opt12 = exprLev2~ opt(("&&"|"||") ~ exprLev1)^^ {
      case e1 ~ Some("&&" ~ e2) => And(e1, e2)
      case e1 ~ Some("||" ~ e2) => Or(e1, e2)
      case e1 ~ None => e1
    }

    /*val opt5 = exprLev2 ~ compOperator~ exprLev1 ^^{
        case e1~">="~e2 => Geq(e1, e2)
        case e1~"<="~e2 => Geq(e2, e1)
        case e1~"=="~e2 => Eq(e1, e2)
        case e1~"!="~e2 => Neq(e1, e2)
        case e1~">"~e2 => Gt(e1, e2)
        case e1~"<"~e2 => Gt(e2, e1)
    }*/

    opt1 | opt2 | opt3 | opt4 | opt5 | opt6 | opt7 | opt8 | opt9 | opt10 | opt11 | opt12
  }

  def exprLev2: Parser[Expr] = {
    exprLev3 ~ opt(compOperator~ exprLev2) ^^{
      case e1~Some(">="~e2) => Geq(e1, e2)
      case e1~Some("<="~e2) => Geq(e2, e1)
      case e1~Some("=="~e2) => Eq(e1, e2)
      case e1~Some("!="~e2) => Neq(e1, e2)
      case e1~Some(">"~e2) => Gt(e1, e2)
      case e1~Some("<"~e2) => Gt(e2, e1)
      case e1~None => e1
    }
  }

  def exprLev3: Parser[Expr] = {
    exprLev4 ~ opt( ("+"| "-") ~ exprLev3 ) ^^ {
      case e1 ~ Some("+" ~ e2) => Plus(e1, e2)
      case e1 ~ Some("-" ~ e2) => Minus(e1, e2)
      case e1 ~ None => e1
    }
  }

  def exprLev4: Parser[Expr] = {

    exprLev5 ~ opt(("*"|"/") ~ exprLev4) ^^ {
      case e1 ~ Some("*" ~ e2) => Mult(e1, e2)
      case e1 ~ Some("/" ~ e2) => Div(e1, e2)
      case e1 ~ None => e1
    }

  }

  def exprLev5: Parser[Expr] = {
    ( floatingPointNumber ^^ { s => Const(s.toFloat)} ) |
      (  "true"^^{ _ => ConstBool(true) } ) |
      ( "false" ^^{ _ => ConstBool(false) } ) |
      (  "(" ~> exprLev1 <~ ")" ) |
      ( ( "rectangle" | "triangle" | "circle" | "line" | "!"  ) ~ ("(" ~> exprLev1 <~ ")") ^^{
        //case "rectangle"~e => Rectangle(e)
        //case "triangle"~e => EquiTriangle(e)
        //case "circle"~e => Circle(e)
        //case "line"~e => Line(e)
        case "!"~e => Not(e)
      } ) |
      ( identifier ~ rep(funCallArgs)  ^^ {
        case s~Nil => Ident(s)
        case s~l => l.foldLeft[Expr] (Ident(s)) { case (e, lj) => FunCall(e, lj) }
      })
  }

  def parseString(s: String): Program = {
    println(s)

    val e= parseAll(exprLev1, s)
    e match {
      case Success(p, _) => TopLevel(p)
      case Failure(msg, _) => throw new IllegalArgumentException("Failure:" + msg)
      case Error(msg, _) => throw new IllegalArgumentException("Error: " + msg)
    }
  }
}

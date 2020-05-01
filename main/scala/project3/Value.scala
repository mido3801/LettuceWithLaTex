package project3

sealed trait Value
case class NumValue(f: Double) extends Value
case class Closure(x: String, e: Expr, env: Environment) extends Value
case class BoolValue(b: Boolean) extends Value
case class FormulaValue(f:MyFormula) extends Value
case class SymbolValue(x:String) extends Value
case class SymNumValue(d:String) extends Value

/* Create some utility functions to operate on values.  */
object ValueOps {

  def minus(v1: Value, v2: Value): Value = (v1, v2) match {
    case (NumValue(f1), NumValue(f2)) => NumValue(f1 - f2)
    case (FormulaValue(f1), FormulaValue(f2)) => FormulaValue(MyFormula(List(FormMinus(f1,f2))))
    case (FormulaValue(f1),SymbolValue(x)) => FormulaValue(MyFormula(List(FormMinus(f1,MyFormula(List(MySym(x)))))))
    case (SymbolValue(x),FormulaValue(f1)) => FormulaValue(MyFormula(List(FormMinus(MyFormula(List(MySym(x))),f1))))
    case (FormulaValue(f1),SymNumValue(d))=> FormulaValue(MyFormula(List(FormMinus(f1,MyFormula(List(MyNum(d)))))))
    case (SymNumValue(d),FormulaValue(f1))=> FormulaValue(MyFormula(List(FormMinus(MyFormula(List(MyNum(d))),f1))))
    case _ => throw new IllegalArgumentException("Illegal minus arguments")
  }


  def geq(v1: Value, v2: Value): Value = (v1, v2) match {
    case (NumValue(f1), NumValue(f2)) => BoolValue(f1 >= f2)
    case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the geq comparator")
  }

  def gt(v1: Value, v2: Value): Value = (v1, v2) match {
    case (NumValue(f1), NumValue(f2)) => BoolValue(f1 > f2)
    case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the eq comparator")
  }

  def equal(v1: Value, v2: Value): Value = (v1, v2) match {
    case (NumValue(f1), NumValue(f2)) => BoolValue(f1 == f2)
    case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 == b2)
    case _ =>  throw new IllegalArgumentException("Cannot compare non numeric/boolean expressions with the eq comparator")
  }

  def notEqual(v1: Value, v2: Value): Value = {
    val v = equal(v1, v2)
    v match {
      case BoolValue(b) => BoolValue(!b)
      case _ => throw new IllegalArgumentException("Internal error: something is really wrong") // This should never happen
    }
  }

  def plus(v1:Value,v2:Value):Value = (v1,v2) match{
    case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
    case (FormulaValue(f1), FormulaValue(f2)) => FormulaValue(MyFormula(List(FormPlus(f1,f2))))
    case (FormulaValue(f1),SymbolValue(x)) => FormulaValue(MyFormula(List(FormPlus(f1,MyFormula(List(MySym(x)))))))
    case (SymbolValue(x),FormulaValue(f1)) => FormulaValue(MyFormula(List(FormPlus(MyFormula(List(MySym(x))),f1))))
    case (FormulaValue(f1),SymNumValue(d))=> FormulaValue(MyFormula(List(FormPlus(f1,MyFormula(List(MyNum(d)))))))
    case (SymNumValue(d),FormulaValue(f1))=> FormulaValue(MyFormula(List(FormPlus(MyFormula(List(MyNum(d))),f1))))
    case (SymbolValue(x),SymNumValue(d)) => FormulaValue(MyFormula(List(FormPlus(MyFormula(List(MySym(x))),MyFormula(List(MyNum(d)))))))
    case (SymNumValue(d),SymbolValue(x)) => FormulaValue(MyFormula(List(FormPlus(MyFormula(List(MyNum(d))),MyFormula(List(MySym(x)))))))
    case _ => throw new IllegalArgumentException("Illegal add arguments")
  }

  def mult(v1:Value,v2:Value):Value= (v1,v2) match{
    case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
    case (FormulaValue(f1), FormulaValue(f2)) => FormulaValue(MyFormula(List(FormMult(f1,f2))))
    case (FormulaValue(f1),SymbolValue(x)) => FormulaValue(MyFormula(List(FormMult(f1,MyFormula(List(MySym(x)))))))
    case (SymbolValue(x),FormulaValue(f1)) => FormulaValue(MyFormula(List(FormMult(MyFormula(List(MySym(x))),f1))))
    case (FormulaValue(f1),SymNumValue(d))=> FormulaValue(MyFormula(List(FormMult(f1,MyFormula(List(MyNum(d)))))))
    case (SymNumValue(d),FormulaValue(f1))=> FormulaValue(MyFormula(List(FormMult(MyFormula(List(MyNum(d))),f1))))
    case _ => throw new IllegalArgumentException("Illegal mult arguments")
  }

  def div(v1:Value,v2:Value):Value= (v1,v2) match{
    case (NumValue(f1),NumValue(f2)) if f2!=0 => NumValue(f1/f2)
    case (NumValue(f1),_) => throw new Exception("cant divide by 0")
    case (FormulaValue(f1), FormulaValue(f2)) => FormulaValue(MyFormula(List(FormDiv(f1,f2))))
    case (FormulaValue(f1),SymbolValue(x)) => FormulaValue(MyFormula(List(FormDiv(f1,MyFormula(List(MySym(x)))))))
    case (SymbolValue(x),FormulaValue(f1)) => FormulaValue(MyFormula(List(FormDiv(MyFormula(List(MySym(x))),f1))))
    case (FormulaValue(f1),SymNumValue(d))=> FormulaValue(MyFormula(List(FormDiv(f1,MyFormula(List(MyNum(d)))))))
    case (SymNumValue(d),FormulaValue(f1))=> FormulaValue(MyFormula(List(FormDiv(MyFormula(List(MyNum(d))),f1))))
    case _ => throw new IllegalArgumentException("Illegal div arguments")
  }



}
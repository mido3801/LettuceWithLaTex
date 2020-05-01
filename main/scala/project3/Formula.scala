package project3

sealed trait Formula{

}
case class MySym(s:String) extends Formula{
  override def toString: String ={
    s
  }
}
case class MyNum(d:String) extends Formula
case class MyExp(s:MySym) extends Formula
case class MySin(s:MySym) extends Formula
case class MyCos(s:MySym) extends Formula
case class MyDeriv(f:Formula) extends Formula{
  def deriveFormula:Formula={
    f match {
    case MySym(x)=> MyNum("1")
    case MyExp(x) => MyExp(x)
    case MySin(x) => MyCos(x)
    case FormPlus(f1,f2) => FormPlus(f1.deriveFormula,f2.deriveFormula)
    case FormMinus(f1,f2) => FormMinus(f1.deriveFormula,f2.deriveFormula)
    case FormMult(f1,f2) => FormPlus(MyFormula(List(FormMult(f2,f1.deriveFormula))),MyFormula(List(FormMult(f1,f2.deriveFormula))))
    case FormDiv(f1,f2) => FormDiv(MyFormula(List(FormMinus(MyFormula(List(FormMult(f2,f1.deriveFormula))),MyFormula(List(FormMult(f1,f2.deriveFormula)))))),MyFormula(List(FormMult(f2,f2))))
    case MyNum(d) => MyNum("0")
    }
  }
}
case class FormPlus(f1:MyFormula,f2:MyFormula) extends Formula
case class FormMinus(f1:MyFormula,f2:MyFormula) extends Formula
case class FormMult(f1:MyFormula,f2:MyFormula) extends Formula
case class FormDiv(f1:MyFormula,f2:MyFormula) extends Formula
case class FormNum(x:Double) extends Formula



case class MyFormula(val formulaList:List[Formula]){
  def toLatex: String =  {
    var checkedList = formulaList.map({
      (current)=> current match {
        case MyDeriv(f)=> MyDeriv(f).deriveFormula
        case _ => current
      }
    })
     var text = checkedList.map({
       (current)=>
         current match{

           //case MyFormula(f)=> MyFormula(f).toLatex
         case MySin(x) => {
           val symVal = x.toString
           s"\\sin{$symVal}"
         }
         case MyCos(x) => {
           val symVal = x.toString
           s"\\cos{$symVal}"
         }

         case MyExp(x) => {
           val symVal = x.toString
           s"\\exp{$symVal}"
         }
         case FormPlus(f1,f2) => {
           val s1 = f1.toLatex
           val s2 = f2.toLatex
           s"$s1 + $s2"
         }
         case FormMinus(f1,f2) => {
           val s1 = f1.toLatex
           val s2 = f2.toLatex
           s"$s1 - $s2"
         }
         case FormMult(f1,f2) => {
           val s1 = f1.toLatex
           val s2 = f2.toLatex
           s"$s1 * $s2"
         }
         case FormDiv(f1:MyFormula,f2:MyFormula) => {
           val s1 = f1.toLatex
           val s2 = f2.toLatex
           s"\\frac{$s1}{$s2}"
         }

         case MyNum(f) => f
         case MySym(x) => x
       }
     })

      val endString = text.mkString
      endString

  }

  def deriveFormula:MyFormula = {
    var derivative = formulaList.map({
      (current)=>current match{
        case MySym(x)=> FormNum(1.0)
        case MyExp(x) => MyExp(x)
        case MySin(x) => MyCos(x)
        case FormPlus(f1,f2) => FormPlus(f1.deriveFormula,f2.deriveFormula)
        case FormMinus(f1,f2) => FormMinus(f1.deriveFormula,f2.deriveFormula)
      }

    })
    MyFormula(derivative)
  }

}
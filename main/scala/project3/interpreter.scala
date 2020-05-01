package project3
  /*
     A Lettuce interpreter with evalExpr function that has missing cases to be handled. See TODOs below.
   */

  object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
      val v1 = evalExpr(expr, env)
      val v2 = evalExpr(expr1, env)
      fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
      case Const(d) => NumValue(d)
      case ConstBool(b) => BoolValue(b)
      case Ident(s) => env.lookup(s)
      case Symbol(s)=> SymbolValue(s)
      case SymNum(s) => SymNumValue(s)
      case Exp(e)=> { e match {
        case Ident(s)=>{
          val innerVal = env.lookup(s)
          innerVal match{
            case SymbolValue(x) => {
              FormulaValue(MyFormula(List(MyExp(MySym(x)))))
               }
            case _ => throw new Exception("Need symbol in exp")
          }
        }
        }
      }
      case Sin(e)=>{e match {
        case Ident(s)=>{
          val innerVal = env.lookup(s)
          innerVal match{
            case SymbolValue(x) => {
              FormulaValue(MyFormula(List(MySin(MySym(x)))))
            }
            case _ => throw new Exception("Need symbol in exp")
          }
        }
      }
      }
      case Cos(e)=>{e match {
        case Ident(s)=>{
          val innerVal = env.lookup(s)
          innerVal match{
            case SymbolValue(x) => {
              FormulaValue(MyFormula(List(MyCos(MySym(x)))))
            }
            case _ => throw new Exception("Need symbol in exp")
          }
        }
      }
      }
      case Deriv(e,sym)=>{
        val innerVal = evalExpr(e,env)
        innerVal match{
          case FormulaValue(f)=>FormulaValue(MyFormula(List(MyDeriv(f.formulaList(0)))))
          case _ => throw new Exception("Can only differentiate formulas")
        }
      }

      //case Latex(e)=>


      case Plus(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.plus)
      case Minus(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.minus)
      case Mult(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.mult)
      case Div(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.div)
      case Geq(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.geq)
      case Gt(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.gt)
      case Eq(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.equal)
      case Neq(e1, e2) => binaryExprEval(e1, e2, env)(ValueOps.notEqual)
      case And(e1, e2) => {
        val v1 = evalExpr(e1, env)
        v1 match {
          case BoolValue(true) => {
            val v2 = evalExpr(e2, env)
            v2 match {
              case BoolValue(_) => v2
              case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
            }
          }
          case BoolValue(false) => BoolValue(false)
          case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
        }
      }

      case Or(e1, e2) => {
        val v1 = evalExpr(e1, env)
        v1 match {
          case BoolValue(true) => BoolValue(true)
          case BoolValue(false) => {
            val v2 = evalExpr(e2, env)
            v2 match {
              case BoolValue(_) => v2
              case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
            }
          }
          case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
        }
      }

      case Not(e) => {
        val v = evalExpr(e, env)
        v match {
          case BoolValue(b) => BoolValue(!b)
          case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
        }
      }

      case IfThenElse(e, e1, e2) => {
        val v = evalExpr(e, env)
        v match {
          case BoolValue(true) => evalExpr(e1, env)
          case BoolValue(false) => evalExpr(e2, env)
          case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
        }
      }


      case Let(x, e1, e2) => {
        val v1 = evalExpr(e1, env)
        val env2 = Extend(x, v1, env)
        evalExpr(e2, env2)
      }

      case FunDef(x, e) => Closure(x, e, env)

      case LetRec(f, x, e1, e2) => {
        val newEnv = ExtendREC(f, x, e1, env)
        evalExpr(e2, newEnv)
      }

      case FunCall(fCallExpr, arg) => {
        println("called function")
        val v1 = evalExpr(fCallExpr, env)
        println(s"v1 is $v1")
        val v2 = evalExpr(arg, env)
        println(s"v2 is $v2")
        v1 match {
          case Closure(x, e, oldEnv) =>
            val newEnv = Extend(x, v2, oldEnv)
            evalExpr(e, newEnv)

          case _ => throw new Exception("not a closure")

        }
      }
    }

    def evalProgram(p: Program): Value = p match {
      case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

  }

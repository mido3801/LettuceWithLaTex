package project3

object formulaTest{
  def execProgram(name:String,fun:()=>String)={
    println(name)
    val v = TestPrograms.parseAndInterpretProgram(fun())
    v match{
      case FormulaValue(x) => {println(x)
        println(x.toLatex)
      }
      case _ => "Error program did not return formula value"
    }
  }

  def main(args: Array[String]): Unit = {
    execProgram("Program1",TestPrograms.program1)
    execProgram("Program2",TestPrograms.program2)
    execProgram("Program3",TestPrograms.program3)
    execProgram("Program4",TestPrograms.program4)
    execProgram("Program5",TestPrograms.program5)
    execProgram("Program6",TestPrograms.program6)
    execProgram("Program7",TestPrograms.program7)
    execProgram("Program8",TestPrograms.program8)
    execProgram("Program9",TestPrograms.program9)
    execProgram("Program10",TestPrograms.program10)
  }

}
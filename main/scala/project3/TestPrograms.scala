package project3

object TestPrograms{
val debug = true

  def program1():String =
    """
      |let x = symbol(x) in
      |exp(x)
      |""".stripMargin

  def program2():String =
    """
      |let x = symbol(x) in
      | let y = exp(x) in
      |   let z = sin(x) in
      |     y-z
      |""".stripMargin

  def program3():String =
    """
      |let a = symbol(a) in
      | let b = exp(a) in
      |   let c = cos(a) in
      |     let d = sin(a) in
      |       b*d+c-b
      |""".stripMargin

  def program4():String = {
    """
      |let addThrice = function (f)
      |   (f+f+f) in
      |   let x = symbol(x) in
      |   addThrice(sin(x))
      |""".stripMargin
  }

  def program5():String = {
    """
      |let x = symbol(x) in
      | let y = exp(x) in
      |   let z = sin(x) in
      |     y/z
      |""".stripMargin
  }

  def program6():String =  {
    """
      |let x = symbol(x) in
      | let y = sin(x) in
      |   derivative(y)
      |""".stripMargin
  }

  def program7():String = {
    """
      |let x = symbol(x) in
      | let y = sin(x) in
      |   let z = sin(x) in
      |     let a = y-z in
      |       derivative(a)
      |""".stripMargin
  }

  def program8():String = {
    """
      |let x = symbol(x) in
      | let y = sin(x) in
      |   let z = exp(x) in
      |     let a = y/z in
      |       derivative(a)
      |""".stripMargin
  }

  def program9():String = {
    """
      |let x = symbol(x) in
      | let derive = function(y)
      |   (derivative(y))
      |   in
      |   let g = sin(x) in
      |   let h = exp(x) in
      |   let j = g + h
      |   in derive(j)
      |""".stripMargin
  }

  def program10():String = {
    """
      |let x = symbol(x) in
      | let y = num(2) in
      |   let z = symbol(y) in
      |   x + y + z
      |""".stripMargin
  }


def parseAndInterpretProgram(s: String): Value = {
  val p: Program = new LettuceParser().parseString(s)
  if (debug) {
    println("--- Debug --- ")
    println(p)
    println("--- Debug ---")
  }
  val v = Interpreter.evalProgram(p)
  if (debug)
    println(s"Returned value : $v")
  v
}
}
package calculator

import calculator.Parsers.{extractArgument, parse_string, shunting_yard, turn_to_RPN}
class CalcBase extends CALCULATOR {
  var variables : Map[String , Expression] = Map()
  var functions : Map[String , Fun] = Map()

  def parse_function(s : String) = {

  }
  override def readEval(command: String): String = {
    try {
      command match {
        case x if x.length > 1 && x(0).isLetter && x(1) == '(' => {
          val arg   = extractArgument(x.slice(1 , x.length))
          val arg_expression = parse(arg._1)
          if(command.contains("=")) {
            if(!functions.contains(command(0).toString))
              functions += (command(0).toString -> Fun(parse(command.split("=")(1)) , command(2).toString))
            else return command(2).toString + " = " + functions(command(0).toString).solve_for_y(parse(command.split("=")(1))).toString +
              " = " + functions(command(0).toString).solve_for_y(parse(command.split("=")(1))).value.toString

            Plot.plot(functions(command(0).toString))
            ""
          }
          else {
            command(0).toString + "(" + arg_expression.toString + ") = " +
            functions(command(0).toString).solve_for_x(evaluate(arg_expression)).toString + " = " +
            functions(command(0).toString).solve_for_x(evaluate(arg_expression)).value.toString
          }
        }
        case x if x.contains("=") => {
          variables += (x.split("=")(0).filterNot(_ == ' ') -> parse(x.split("=")(1)))
          ""
        }
        case _ => {
          val RPN = parse(command)
          evaluate(RPN).toString + " => " + evaluate(RPN).value.toString
        }
      }
    }
  }
  def parse(command : String) : Expression = {

    val parsed = parse_string(command)
//    println("Parsed :   " + parsed.mkString(" "))
    val shunted = shunting_yard(parsed)
//    println("shunted :   " + shunted.mkString(" "))
    val RPN = turn_to_RPN(shunted)
    RPN
  }

  def evaluate(exp : Expression) : Expression = {
    var ex : Expression = exp
    var temp = exp
    ex = eval(exp)
    while(temp != ex) {
      temp = ex
      ex = eval(ex)
    }
    ex

  }
  def eval(exp : Expression) : Expression = {
    var temp : Expression = null

    exp match {
      case Radical(Constant(0) , b) => Constant(0)
      case Sin(x) => Sin(evaluate(x))
      case Cos(x) => Cos(evaluate(x))
      case Tan(x) => Tan(evaluate(x))
      case Ln(x) if x.isInstanceOf[Euler] => Constant(1)
      case Ln(x) => Ln(evaluate(x))
      case Log(x,y) if y.isInstanceOf[Euler] => Ln(x)
      case Log(x,y) => Log(evaluate(x) , evaluate(y))
      case Variable(x) if variables.contains(x) => variables(x)


      case Operator(Negative(x) , op , Negative(y)) if  op.s == "*" || op.s == "/" => Operator(x , op , y)
      case Fraction(Negative(x) , Negative(y)) => Fraction(x , y)

      case Log(Operator(x , Op(sign) , y) , b) if sign == "/" || sign == "*" =>
        if(sign == "*") Operator(Log(x , b) , Op("+") , Log(y , b)) else Operator(Log(x , b) , Op("-") , Log(y , b))
      case Log(Exponent(x , y) , b) => Operator(y , Op("*") , Log(x , b))
      case Exponent(x , y) => evaluate(x)^evaluate(y) match {
        case Some(i) => i
        case None => Exponent(x , y)
      }
      case Operator(lhs , op , rhs) => do_operation(evaluate(lhs) , op , evaluate(rhs)) match {
        case Some(i) => i
        case None => Operator(evaluate(lhs) , op , evaluate(rhs))
      }
      case _ => exp
    }
  }
  def do_operation(lhs : Expression, op : Op, rhs : Expression): Option[Expression] = {
    op match {
      case Op(x) if x == "+" => lhs + rhs
      case Op(x) if x == "-" => lhs - rhs
      case Op(x) if x == "*" => lhs * rhs
      case Op(x) if x == "/" => lhs / rhs

    }
  }

  //TODO  case class REPL_Variable(s : String)

  def can_do_operation[A <: Expression, B <: Expression](lhs : A  , op : Op ,  rhs : B) : Boolean = {
    rhs match {
      case x : Constant => lhs match {
        case y : Constant => true
        case _ => false
      }
      case x  if x.getClass == lhs.getClass => true
      case _ => false
    }
  }




}

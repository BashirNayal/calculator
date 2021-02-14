package calculator

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Parsers {
  def isMathematicalConstruct(s : String) : Boolean = {
//        s = s.toLowerCase()
    s.startsWith("log") || s.startsWith("sin") || s.startsWith("cos") || s.startsWith("sec") || s.startsWith("tan") || s.startsWith("ln") || s.startsWith("rad")
  }
  def turn_to_expression(s : String) : Expression = {
    val parsed = parse_string(s)
    val shunted = shunting_yard(parsed)
    val RPN = turn_to_RPN(shunted)
    RPN

  }
  def turn_to_RPN(arr : Array[Expression]) : Expression = {
    val stack : mutable.Stack[Expression] = mutable.Stack()
    for(i <- arr.indices) {

      arr(i) match {
        case Op(x) =>
          val rhs : Expression = stack.pop()
          val lhs : Expression = stack.pop()
          val res : Expression = Operator(lhs , Op(x) , rhs)
          stack.push(res)
        case Constant(x) =>
          stack.push(Constant(x))

        case _ => stack.push(arr(i))

      }
    }
    stack.top
  }

  def extractArgument(s : String) : (String , Int) = {
    var braces : Int = 0
    var current : Int = 0
    while(current < s.length) {
      if(s(current) == '(') braces += 1
      else if(s(current) == ')') braces -= 1
      if(braces == 0) {
        return (s.slice(1 , current) , current)
      }
      current += 1
    }
    null
  }

  def get_mathematical_expression(construct : String , argument : String , base : Expression) : Expression = {
    construct.toLowerCase() match {
      case "ln" => Ln(turn_to_expression(argument))
      case "log" => Log(turn_to_expression(argument) , base)
      case "cos" =>  Cos(turn_to_expression(argument))
      case "sin" =>  Sin(turn_to_expression(argument))
      case "tan" => Tan(turn_to_expression(argument))
      case "rad" => Radical(turn_to_expression(argument) , base)
      case "sec" => Sec(turn_to_expression(argument))
    }
  }
  def get_expression_name(s : String) : String = {
    if      (s.startsWith("log")) "log"
    else if (s.startsWith("sin")) "sin"
    else if (s.startsWith("cos")) "cos"
    else if (s.startsWith("sec")) "sec"
    else if (s.startsWith("tan")) "tan"
    else if (s.startsWith("rad")) "rad"
    else if (s.startsWith("ln"))  "ln"
    else ""

  }
  def extract_base(n : String, s : String): (Expression,Int) = {
    var current = 0
    if(s(current) != ',' && n == "log") (Constant(10),0)
    else if(s(current) != ',' && n == "rad") (Constant(2) , 0)
    else {
      current += 1
      while(current < s.length && s(current) != ',') current += 1
      (turn_to_expression(s.slice(1 , current)) , current + 1)
    }

  }
  def parse_string(s : String) : Array[Expression] = {
    var res : ArrayBuffer[Expression] = ArrayBuffer()
    var is_exponent = false
    var negative : Boolean = false
    var expecting_operand : Boolean = true
    var current : Int = 0
    while(current < s.length) {
      if(current + 1 < s.length && s(current) == 'p' && s(current + 1) == 'i') {
        res = res :+ Pi()
        current += 1
      }
      else if(isMathematicalConstruct(s.slice(current , s.length - 1))) {////***************////
        expecting_operand = false

        val expression_name = get_expression_name(s.slice(current , s.length - 1))
        var base : (Expression , Int) = (Constant(10) , 0)      //this is used for logarithms
        if(expression_name == "log" || expression_name == "rad") {
          base = extract_base(expression_name , s.slice(current + expression_name.length , s.length))
          current += base._2
        }

        val argument = extractArgument(s.drop(current + expression_name.length))
        if(negative) {
          res = res :+ Negative(get_mathematical_expression(expression_name , argument._1 , base._1))
          negative = false
        }
        else {
          res = res :+ get_mathematical_expression(expression_name , argument._1 , base._1)
        }
        if(is_exponent) {
          val power : Expression = res(res.length - 1)
          val exponent : Expression = res(res.length - 2)
          res = res.dropRight(2)
          res = res :+ Exponent(exponent , power)
          is_exponent = false
        }

        current += expression_name.length + argument._2
      }
      else if(s(current).isDigit) {
        expecting_operand = false
        val digit_index = current
        current += 1
        while(current < s.length && s(current).isDigit) current += 1
        if(negative) {
          res = res :+ Negative(Constant(s.slice(digit_index , current).toInt))
          negative = false
        }
        else res = res :+ Constant(s.slice(digit_index , current).toInt)
        if(is_exponent) {
          val power : Expression = res(res.length - 1)
          val exponent : Expression = res(res.length - 2)
          res = res.dropRight(2)
          res = res :+ Exponent(exponent , power)
          is_exponent = false
        }

        current -= 1
      }
      else if(is_symbol(s(current))) {
        if (s(current) == '(') res = res :+ Symbol("(")
        else if (s(current) == ')') res = res :+ Symbol(")")
        else if (is_operator(s(current))) {
          if (s(current) == '+') res = res :+ Op("+")
          else if (s(current) == '-') {
            if (expecting_operand) negative = true
            else res = res :+ Op("-")
          }
          else if (s(current) == '/') res = res :+ Op("/")
          else if (s(current) == '*') res = res :+ Op("*")
          else if (s(current) == '^') is_exponent = true
          expecting_operand = true

        }
      }
      else if (s(current) == 'e') res = res :+ Euler()
      else if(s(current) != ' '){
        expecting_operand = false
        res = res :+ Variable(extract_variable_name(s.slice(current , s.length)))
        current += res.last.asInstanceOf[Variable].s.length - 1
      }
      current += 1
    }
    res.toArray

  }
  def extract_variable_name(s : String) : String = {
    var count = 0
    while(count < s.length && s(count) != ' ' && !is_symbol(s(count))) count += 1
    s.slice(0 , count)
  }
  def is_symbol(c : Char) : Boolean = is_operator(c) || c == ')' || c == '('

  def turn_to_Array(s : String) : Array[String] = {
    var current = 0
    var res : ArrayBuffer[String] = ArrayBuffer()
    while(current < s.length) {
      if(current + 3 <= s.length && isMathematicalConstruct(s.slice(current , current + 3))) {
        res = res :+ s.slice(current , current + 3)
        current += 2
      }
      else if(s(current) != ' ')res = res :+ s(current).toString
      current += 1
    }
    res.toArray
  }
  def is_operator(c : Char) : Boolean = c == '+' || c == '-' || c == '*' || c == '/' || c == '^'

  //http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
  def shunting_yard(exp : Array[Expression]) : Array[Expression] = {

    val stack : mutable.Stack[Expression] = mutable.Stack()
    var arr : Array[Expression] = Array()
    for (i <- exp.indices) {
      exp(i) match {
        case Symbol(x) if x == "(" => stack.push(Symbol(x))
        case Symbol(x) if x == ")" =>
          while(stack.top.toString != "(") arr = arr :+ stack.pop()
          stack.pop()
        //Can handle mismatching parentheses here
        case Op(x)  =>
          if(stack.isEmpty || stack.top.toString == "(") stack.push(Op(x))
          else {
            stack.top match {
              case top : Op =>
                if(Op(x).precedence >= top.precedence) stack.push(Op(x))
                else if(Op(x).precedence <= top.precedence && Op(x).associativity == "left") {
                  while (stack.nonEmpty && Op(x).precedence <= top.precedence && (Op(x).associativity == "left" /*|| Op(x).associativity == "both"*/)) arr = arr :+ stack.pop()
                  stack.push(exp(i))
                }
            }
          }
        case _ => arr = arr :+ exp(i)
      }
    }
    while (stack.nonEmpty) arr = arr :+ stack.pop()
    arr
  }
}

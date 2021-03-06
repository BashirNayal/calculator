package calculator

abstract class Expression {
  def value : Double = Double.NaN
  def +(lhs : Expression) : Option[Expression] = None
  def -(lhs : Expression) : Option[Expression] = None
  def *(lhs : Expression) : Option[Expression] = None
  def /(lhs : Expression) : Option[Expression] = None
  def ^(lhs : Expression) : Option[Expression] = None
}
case class Symbol(s : String) extends Expression {
  override def toString: String = s
}

case class Op(s : String) extends Expression {
  override def toString: String = s
  def precedence : Int = {
    s match {
      case "^" => 4
      case "*" => 3
      case "/" => 3
      case "+" => 2
      case "-" => 2
      case _ => 0
    }
  }
  def associativity : String = {
    s match {
      case "^" => "right"
      case "*" => "left"
      case "/" => "left"
      case "+" => "left"
      case "-" => "left"
      case _ => ""
    }
  }

}


case class Negative(x : Expression) extends Expression {
  override def value: Double = -x.value

  override def toString: String = "-" + x.toString


}
case class Euler() extends Expression {
  override def value : Double = 2.71828
  override def toString : String = "e"
}

case class Constant(x : Double) extends Expression {
  override def value: Double = x
  override def toString: String = x.toString
  override def +(rhs : Expression) : Option[Expression] = rhs match {
    case Constant(y) => Some(Constant(x + y))
    case Fraction(n , d) => Fraction(n , d) + Fraction(Constant(x) , Constant(1))
    case _ => None
  }
  override def -(rhs : Expression) : Option[Expression] = rhs match {
    case Constant(y) => Some(Constant(x - y))
    case _ => None
  }
  override def *(rhs : Expression) : Option[Expression]  = rhs match {
    case Constant(y) => Some(Constant(x * y))
    case _ => None
  }
  override def /(rhs : Expression) : Option[Expression]  = rhs match {
    case Constant(y) => if(x % y != 0) Some(Fraction(Constant(x) , Constant(y))) else Some(Constant(x / y))
    case e : Expression => Some(Fraction(this , e))
    case _ => None
  }
  override def ^(rhs : Expression) : Option[Expression] = rhs match {
    case Constant(y) =>
      Some(Constant(Math.pow(this.value , rhs.value)))
    case _ => None
  }
}
case class Variable(s : String) extends Expression {
  override def toString: String = s
}
case class Operator(lhs : Expression , op : Op , rhs : Expression) extends Expression {
  override def toString: String = {
    this match {
      case Operator(Operator(lhsL , opL , rhsL) , op , Operator(lhsR , opR , rhsR)) if op.precedence > opL.precedence && op.precedence > opR.precedence =>
        "( " + lhsL.toString + " " +  opL.toString + " " +  rhsL.toString + " ) " +  op.toString + " ( " + lhsR.toString + " " + opR.toString + " " + rhsR.toString + " )"

      case Operator(lhs , op ,Operator(lhsR , opR , rhsR)) if op.precedence > opR.precedence =>
        lhs.toString + " " + op.toString + " ( " + lhsR.toString + " " + opR.toString + " " + rhsR.toString + " )"

      case Operator(Operator(lhsL , opL , rhsL) , op , rhs) if op.precedence > opL.precedence =>
        "( " + lhsL.toString + " " +  opL.toString + " " +  rhsL.toString + " ) " +  op.toString + " " + rhs.toString

      case _ => lhs.toString + " " + op.toString + " " + rhs.toString
    }
  }
  override def value : Double = do_operation(lhs.value , op , rhs.value)

  def do_operation(lhs : Double, op : Op, rhs : Double): Double = {
    op match {
      case Op(x) if x == "+" => lhs + rhs
      case Op(x) if x == "-" => lhs - rhs
      case Op(x) if x == "*" => lhs * rhs
      case Op(x) if x == "/" => lhs / rhs

    }
  }
}

case class Log(x : Expression , base : Expression = Constant(10)) extends Expression {
  override def value: Double = Math.log10(x.value) / Math.log10(base.value)
  override def toString: String = {
    "log" + base.toString + "(" + x.toString + ")"
  }
  override def +(rhs : Expression) : Option[Expression]  = rhs match {
    case Log(Constant(y) , b) if b == base && x.isInstanceOf[Constant] => Some(Log(Operator(x , Op("*") , Constant(y)), base))
    case _ => None
  }
  override def -(rhs : Expression) : Option[Expression] = rhs match {
    case Log(Constant(y) , b) if b == base && x.isInstanceOf[Constant] => Some(Log(Operator(x , Op("/") , Constant(y)), base))
    case _ => None

  }
  override def *(rhs : Expression) : Option[Expression] = rhs match {
    case _ => None
  }

  def base_switch : Expression = Fraction(Constant(1) , Log(base , x))
  def base_change(arg : Expression) : Expression = Fraction(Log(x , arg) , Log(base , arg))
}
case class Ln(x : Expression) extends Expression {
  override def value: Double = Math.log10(x.value) / Math.log10(Euler().value)


  override def toString: String = "ln(" + x + ")"

}
case class Fun(x : Expression , v : String) extends Expression {

  override def toString: String =  x.toString
  def solve_for_x(exp : Expression) : Expression = {
    val calc = new CalcBase
    calc.variables += (v -> exp)
    calc.evaluate(x)
  }
  def solve_for_y(exp : Expression): Expression = {
    val calc = new CalcBase
    calc.evaluate(Fun(Operator(x , Op("-") , exp) , v))
    var rhs : Expression = exp
    var lhs : Expression = x
    var temp : Expression = null
    while(temp != lhs) {
      temp = lhs
      lhs match {
        case Exponent(l , b) if !contains_a_var(b) =>
          rhs = Radical(rhs, b)
          lhs = l

        case Operator(l  , Op(sign) , e) if !contains_a_var(l) =>
          rhs = if(sign == "*") Fraction(calc.evaluate(rhs)  , calc.evaluate(e))
          else if(sign == "/") Operator(calc.evaluate(rhs) , Op("*") , calc.evaluate(Negative(e)))
          else if(sign == "+") Operator(calc.evaluate(rhs) , Op("-") , calc.evaluate(e))
          else Operator(calc.evaluate(rhs) , Op("+") , calc.evaluate(e))

          lhs = e

        case Operator(l  , Op(sign) , e) if !contains_a_var(e) =>
          rhs = if(sign == "*") Fraction(calc.evaluate(rhs)  , calc.evaluate(e))
          else if(sign == "/") Operator(calc.evaluate(rhs) , Op("*") , calc.evaluate(Negative(e)))
          else if(sign == "+") Operator(calc.evaluate(rhs) , Op("-") , calc.evaluate(e))
          else Operator(calc.evaluate(rhs) , Op("+") , calc.evaluate(e))
          lhs = e

        case Fraction(l , r) if !contains_a_var(l) =>
          rhs = calc.evaluate(l)
          lhs = Operator(calc.evaluate(r) , Op("*") , calc.evaluate(lhs))

        case Cos(x) if !contains_a_var(x) =>
          rhs = Constant(Math.acos(rhs.value))
          lhs = x

        case Sin(x) if !contains_a_var(x) =>
          rhs = Constant(Math.asin(rhs.value))
          lhs = x

        case Tan(x) if !contains_a_var(x) =>
          rhs = Constant(Math.atan(rhs.value))
          lhs = x

        case Log(x , b) =>
          rhs = Constant(Math.pow(b.value , x.value))
          lhs = x
        case Ln(x) =>
          rhs = Constant(Math.pow(Euler().value , x.value))

        case _ => lhs = lhs
      }
    }



    def contains_a_var(v : Expression) : Boolean = {
    v match {
      case Operator(l , Op(op) , r) if op == "+" || op == "-"=> contains_a_var(l) && contains_a_var(r)
      case Fraction(t,b) => contains_a_var(t) && contains_a_var(b)
      case Log(l , _) => contains_a_var(l)
      case Sin(l) => contains_a_var(l)
      case Cos(l) => contains_a_var(l)
      case Tan(l) => contains_a_var(l)
      case Exponent(l,b) => contains_a_var(l) && contains_a_var(b)
      case Radical(l,r) => contains_a_var(l) && contains_a_var(r)
      case Pi() => false
      case Variable(_) => true
      case Constant(_) => false
      case _ => false

    }
  }
    rhs

  }


}
case class Exponent(x : Expression , power : Expression) extends Expression {
  override def value: Double = Math.pow(x.value , power.value)

  override def ^(rhs : Expression) : Option[Constant] = {
    x match {
      case Constant(b) => rhs match {
        case Constant(p) =>
          Some(Constant(Math.pow(b , p)))

      }
      case _ => None
    }
  }
  override def toString: String = "(" + x.toString + ")" + "^" + power.toString

}
case class Radical(x : Expression , root : Expression = Constant(2)) extends Expression {
  override def toString: String = root.toString + "√(" + x.toString + ")"

  override def value: Double = Math.pow(x.value , 1/root.value)


}
case class Sin(x : Expression) extends Expression {
  override def value: Double = Math.sin(x.value)
}
case class Fraction(numerator : Expression , dominator : Expression) extends Expression {
  override def toString: String =  numerator.toString + " / " + dominator.toString
  override def +(rhs : Expression) : Option[Expression] = rhs match {
    case Fraction(n , d) if d == dominator => Some(Fraction(n + numerator match {
      case Some(i) => i
      case None => Operator(numerator , Op("+") , n)
    } , d))
    case _ => None
  }


  override def value: Double = numerator.value / dominator.value

}
case class Cos(x : Expression) extends  Expression {
  override def value: Double = Math.cos(x.value)
}
case class Sec(x : Expression) extends Expression {
  override def value: Double = 1/Math.cos(x.value)
}
case class Tan(x : Expression) extends Expression {
  override def value: Double = Math.tan(x.value)
}
case class Pi() extends Expression {
  override def toString: String = "π"
  override def value: Double = 3.141592653589793238

}

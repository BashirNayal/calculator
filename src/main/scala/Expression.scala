package calculator


abstract class Expression {
//  def value : Int
  def simplify : Expression = null
  def +(lhs : Expression) : Option[Expression] = None
  def -(lhs : Expression) : Option[Expression] = None
  def *(lhs : Expression) : Option[Expression] = None
  def /(lhs : Expression) : Option[Expression] = None
  def ^(lhs : Expression) : Option[Expression] = None
}
case class Symbol(s : String) extends Expression {
  override def toString: String = s
//  override def value: Int = 0
  override def simplify: Expression = null
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

//  override def value: Int = 0
  override def simplify: Expression = null
}


case class Negative(x : Expression) extends Expression {
//  override def value: Int = -x.value
  //    def without_negation : Expression = x

  override def toString: String = "-" + x.toString

  override def simplify: Expression = Negative(x.simplify)
}
case class Euler() extends Expression {
//  override def value: Int = 0 //TODO
  override def toString : String = "e"
  override def simplify: Expression = this
}

case class Constant(x : Int) extends Expression {
  override def toString: String = x.toString
  override def +(rhs : Expression) : Option[Expression] = rhs match {
    case Constant(y) => Some(Constant(x + y))
    case Fraction(n , d) => Fraction(n , d) + Fraction(Constant(x) , Constant(1))
//    case r : Log => Some(Fraction(this , r))
    case _ => None
  }
  override def -(rhs : Expression) : Option[Expression] = rhs match {
    case Constant(y) => Some(Constant(x - y))
//      case
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
    case Constant(y) => {
      var res = x
      for(_ <- 1 until y) {
        res *= x
        if(res < 0) return None
      }
      Some(Constant(res))
    }
    case _ => None
  }
//  override def value: Int = x
}
case class Variable(s : String) extends Expression {
  override def toString: String = s

//  override def value: Int = 0//TODO
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
//  override def value: Int = 0
//  override def simplify: Expression = this match {
//    case Operator(Operator(lhs_l , op_l , rhs_l) , op , Operator(lhs_r , op_r , rhs_r)) if op.s == "*" && op_l.precedence == 2 && op_r.precedence == 2 =>
//  }
}
case class Log(x : Expression , base : Expression = Constant(10)) extends Expression {
  override def toString: String = {
//    val b : String = if(base.value != 10) base.toString else ""
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
//  override def *(rhs : Expression) : Option[Expression]  = rhs match {
////    case y : Log if y.base == this.base => Some(Operator(this , Op("*") , Constant(2)))
//    case Log(Constant(y) , b) if b == base =>
//
//  }

  def base_switch : Expression = Fraction(Constant(1) , Log(base , x))
  def base_change(arg : Expression) : Expression = Fraction(Log(x , arg) , Log(base , arg))
//  override def value: Int = 0
  override def simplify: Expression = this
}
case class Ln(x : Expression) extends Expression {
  //    override def +(rhs : Expression) : Expression = {
  //      Ln(Constant(999))
  //    }
//  override def value: Int = 0

  override def toString: String = "ln(" + x + ")"

  override def simplify: Expression = this
}
case class Fun(x : Expression , v : String) extends Expression {

  override def toString: String =  x.toString
  def solve_for(exp : Expression) : Expression = {
    val calc = new CalcIntegral
    calc.variables += (v -> exp)
    calc.evaluate(x)
  }
//  override def value: Int = 0
  override def simplify: Expression = ???
}
case class Exponent(x : Expression , power : Expression) extends Expression {
   def value: Int = {
    x match {
      case Constant(b) => power match {
        case Constant(p) =>
          var res = b
          for(_ <- 0 until p) res = res + b
          res

      }
      case _ => 0
    }

  }
  override def toString: String = "(" + x.toString + ")" + "^" + power.toString

  override def simplify: Expression = ???
}
case class Radical(x : Expression , root : Expression = Constant(2)) extends Expression {
  override def toString: String = "√(" + x.toString + ")^(" + root.toString + ")"

  override def simplify: Expression = ???
}
case class Sin(x : Expression) extends Expression {
//  override def value: Int = 0
  override def simplify: Expression = ???
}
case class Fraction(numerator : Expression , dominator : Expression) extends Expression {
  override def toString: String =  numerator.toString + " / " + dominator.toString
  override def +(rhs : Expression) : Option[Expression] = rhs match {
    case Fraction(n , d) if d == dominator => Some(Fraction(n + numerator match {
      case Some(i) => i
      case None => Operator(numerator , Op("+") , n)
    } , d))
//    case x : Constant => x + this
    case _ => None
  }

  override def simplify: Expression = ???
}
case class Cos(x : Expression) extends  Expression {
//  override def value: Int = 0
  override def simplify: Expression = ???
}
case class Sec(x : Expression) extends Expression {
//  override def value: Int = 0
  override def simplify: Expression = ???
}
case class Tan(x : Expression) extends Expression {
//  override def value: Int = 0
  override def simplify: Expression = ???
}
case class Pi() extends Expression {
  override def toString: String = "π"

  override def simplify: Expression = this
}

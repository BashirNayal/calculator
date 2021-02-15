package calculator

import processing.core.PApplet

object main {



  def main(args: Array[String]) {
    println(args.mkString(" "))
    val calc = new CalcBase
    calc.run()
  }
}
//added a comment
package calculator

import processing.core.PApplet

object main {





  def main(args: Array[String]) {
    println(args.mkString(" "))
    val calc = new CalcIntegral
    calc.run()

    val processingArgs = Array("GUI")
    val mySketch = new GUI()
//    PApplet.runSketch(processingArgs, mySketch)

  }
}
//added a comment
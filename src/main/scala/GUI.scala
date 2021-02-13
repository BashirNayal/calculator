package calculator
import processing.core.PApplet

class GUI extends PApplet {

  override def settings(): Unit = {
    size(800, 800)
  }

  override def draw(): Unit = {
    background(190)
    fill(128)
    draw_axis(-0,-0)

    draw_graph(null)
  }
  def draw_axis(x : Int , y : Int) {
    line(width/2 + x , 0 , width/2 + x, height)
    line(0 , height/2 - y , width , height / 2 - y) // minus because draw() has it's axis center at the top left
  }
  def draw_graph(f : Fun): Unit = {
    var x = -250
    while(x < 1000) {
//       line(x ,f.solve_for(x) , x+1 , f.solve_for(x+1))
      val arr1 = translate_coords(-x , -x*(-x))
      val arr2 = translate_coords(1+(-x) , ((1)+(-x))*(-x+1))
//      line(x , x*x , 1+x , ((1+x)*(x+1)))
      line(arr1(0) , arr1(1) , arr2(0) , arr2(1))
      x += 1

    }
  }
  def translate_coords(x : Int , y : Int) : Array[Int] = {
    var arr = new Array[Int](2)
    arr(0) =  x + width/2
    arr(1) = y - height/2
    arr
  }
}

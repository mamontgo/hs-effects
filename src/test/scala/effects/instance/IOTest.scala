package effects.instance

import effects.instance.All._
import org.scalatest.funsuite.AnyFunSuite

class IOTest extends AnyFunSuite {


  trait Hello

  class HelloOne(n: String) extends Hello

  class HelloTwo extends HelloOne("helloTwo")

  def test(xy: => Hello): String = {
    xy.toString
  }

  test("IO combine") {
//    val f = scala.io.Source.fromFile("file.txt")
    val x = IO.create(Seq(1,2,3))
    val y = IO.create(Seq(4,5,6))
    val m = x.map(_.monoid).combine(y)

    println(test(new Hello {}))
    println(test(HelloOne("a")))
    println(test(HelloTwo()))


    m >>= (l => {
      assert(l == Seq(1,2,3,4,5,6))
      IO.create(l)
    })



  }
}

package effects.syntax

import effects.syntax.FunctionSyntax.*
import org.scalatest.funsuite.AnyFunSuite

class FunctionApplicationTest extends AnyFunSuite {

  test("apply function") {
    val addOne: Int => Int = _ + 1
    val x = addOne <-> 2 + 22
    assert(x == 25)
    val y = addOne withParam 34 + 22
    assert(y == 57)
    addOne <-> addOne(1)
    addOne <-> (addOne <-> addOne(1))
  }

  test("flip function") {
    val div: (Double, Double) => Double = _ / _
    assert(div(5, 10) == 0.5) // 0.5
    assert(flip(div)(5, 10) == 2.0) // 2.0
    assert(div.flip(5,10) == 2.0)
  }

  test("const function") {

    val f = const(0)
    assert(f("Hello") == 0)
    assert(f(Seq(1,2,3)) == 0)
    assert(f(3242) == 0)


  }

}

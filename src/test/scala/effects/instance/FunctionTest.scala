package effects.instance

import effects.instance.FunctionInstances.*
import org.scalatest.funsuite.AnyFunSuite

class FunctionTest extends AnyFunSuite {

  val addFive: Double => Double = _ + 5
  val divTwo: Double => Double = _ / 2
  val toStringOut: Double => String = _.toString

  test("function map test") {
    assert(addFive.map(divTwo)(10) == 7.5)
    assert(divTwo.map(addFive)(10) == 10.0)
    assert(addFive.map(toStringOut)(5) == "10.0")

  }

  test("function applicative test") {
    val res = addFive <*> (x => x + _)
    println(res(3))
    assert(res(3) == 11.0)
  }

  test("function flatmap test") {
    val res = addFive.flatMap(x => x + _)(3)
    println(res)
    assert(res == 11.0)
  }

  test("functor consumer") {
    val x = () => 1
    val y =  (_:Int) + 1
    val res = x.map(y)()
    assert(res == 2)
  }
}

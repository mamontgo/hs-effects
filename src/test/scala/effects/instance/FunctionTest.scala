package effects.instance

import effects.All.*
import effects.Monad
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
    val y = (_: Int) + 1
    val res = x.map(y)()
    assert(res == 2)
  }

  test("zipWith function") {
    val subtract = curry(flip((_: Int) - (_: Int)))
    val sTwo: Int => Int = subtract(2)
    val sTen = subtract(10)
    val f = sTwo.zipWith(sTen)(curry((_: Int) + (_: Int)))

//    IO.runEffect(println(f(20)))
    assert(f(20) == 28)

  }

  test("zipWith producer function") {
    val two = () => 2
    val three = () => 3
    val f = two.zipWith(three)(curry((_: Int) + (_: Int)))
    assert(f() == 5)

  }

  test("sequence functions?") {
    val l:Seq[Int => Int] = (1 to 10).map(x => x+(_:Int))
    val x: Int => Seq[Int] = Monad.sequence(l)


  }
}
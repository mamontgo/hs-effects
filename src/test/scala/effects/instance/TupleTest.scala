package effects.instance

import org.scalatest.funsuite.AnyFunSuite
import effects.All._

class TupleTest extends AnyFunSuite {

  test("tuple map") {
    assert(("Hello", 1).map(_+2) == ("Hello", 3))
  }

  test("tuple flatmap") {
    assert(("Hello", 1).flatMap(x => ("From Flatmap", x + 2)) == ("From Flatmap", 3))
  }

  test("tuple monoid combine") {
    assert(("One", Seq(1,2)).combine(("Two", Seq(3,4))) == ("Two", Seq(1,2,3,4)))
  }

  test("tuple applicative") {
    assert((1, "Hello").ap((2, (_:String)+" World")) == (2, "Hello World"))
  }

  test("tuple fold") {
    assert((1, "World").foldLeft("Hello ")(_+_) == "Hello World")
  }

  test("tuple traverse") {
    val e: Either[String, Int] = Right(123)
    assert(("Hello", e).traverse(identity) == Right(("Hello", 123)))
  }

}

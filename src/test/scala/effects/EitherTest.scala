package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*

import scala.language.postfixOps

class EitherTest extends AnyFunSuite {

  test("sequence either items") {
    val x:Seq[Either[String, Int]] = Seq(Right(1), Right(2), Right(3))

    val s:Either[String, Seq[Int]] = sequence(x)

//    IO.runEffect(println(s))

    assert(s == Right(Seq(1, 2, 3)))

  }

  test("sequence either items with left item") {
    val x:Seq[Either[String, Int]] = Seq(Right(1), Right(2), Left("Oops"))
    val s = sequence(x)
    assert(s == Left("Oops"))
  }

  test("sequence either items with multiple left item") {
    val x:Seq[Either[String, Int]]  = Seq(Right(1), Left("Oops"), Right(2), Left("Never here!"))
    val s = sequence(x)
    assert(s == Left("Oops"))
  }

  test("Either map right success") {
    val x:Either[String, Int] = Right(123)
    val res = x <\> (_+1)
    assert(res == Right(124))
  }


  test("Either map left success") {
    val x: Either[String, Int] = Left("Oops")
    val res = x <\> (_ + 1)
    assert(res == Left("Oops"))
  }

  test("right applicative success") {
    val res = Right(434) <*> Right((x:Int) => x+123)
    assert(res == Right(557))
  }

  test("compose right with applicative application") {
    val add = (a:Int) => (b:Int) => a + b
    val result:Either[String, Int] = Right(10) <*> (Right(100) <\> add)

  }

  test("right with some applicative helper") {
    val add = (a:Int, b:Int) => a+b
    val f = curry(add)
    val r =  Right(10) <*> (Right(123) <\> f)
    assert(r == Right(133))

  }

}

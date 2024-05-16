package effects.instance

import effects.Functor.liftF
import effects.All._
import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {

  test("test functor functions") {
    val addOne: Int => Int = _+1
    val f = liftF(addOne)
    assert(f(Right(10)).getOrElse(0) == 11)
    assert(f(Left(10)).getOrElse(0) == 0)
  }


  test("monoid either test") {

    val f: Either[String, Seq[Int]] = Right(Seq(1, 2))
    val s = Right(Seq(3, 4))

    IO.runEffect(println(f.map(_.monoid).combine(s)))
    assert(f.map(_.monoid).combine(s) == Right(Seq(1, 2, 3, 4)))
  }
}

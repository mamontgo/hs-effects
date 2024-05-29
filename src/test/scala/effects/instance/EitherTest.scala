package effects.instance

import effects.Functor.liftF
import effects.All._
import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {

  test("test functor functions") {
    val addOne: Int => Int = _ + 1
    val f = liftF(addOne)
    assert(f(Right(10)).getOrElse(0) == 11)
    assert(f(Left(10)).getOrElse(0) == 0)
  }


  test("monoid either test") {
    val f: Either[String, Seq[Int]] = Right(Seq(1, 2))
    val s = Right(Seq(3, 4))

    IO.runEffect(println(f.combine(s)))
    assert(f.combine(s) == Right(Seq(1, 2, 3, 4)))
  }

  test("fold either test") {
    val x: Either[String, Int] = Right(10)
    val y: Either[String, Int] = Left("Not Today!")
    assert(x.foldLeft(22)(_ + _) == 32)
    assert(y.foldLeft(22)(_ + _) == 22)
  }

  test("traverse either test") {
    val x: Either[String, Seq[Int]] = Right(1 to 3)
    val y = x.traverse(identity)
    assert(y == Seq(Right(1), Right(2), Right(3)))
  }
}
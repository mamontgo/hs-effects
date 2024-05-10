package effects.instance

import effects.Functor.liftF
import effects.instance.All._
import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {

  test("test functor functions") {
    val addOne: Int => Int = _+1
    val f = liftF(addOne)
    assert(f(Right(10)).getOrElse(0) == 11)
    assert(f(Left(10)).getOrElse(0) == 0)
  }
}

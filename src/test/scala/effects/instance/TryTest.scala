package effects.instance

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class TryTest extends AnyFunSuite {

  test("map try") {
    val t:Try[Int] = Success[Int](12)
    assert(t.map(_+1) == Success(13))

    val t2: Try[Int] = Failure[Int](new Exception("Oops"))
    assert(t2.map(_ + 1) == t2)
  }

  test("flatmap try") {
    val t: Try[Int] = Success[Int](12)
    assert(t.flatMap(x => Success(x+1)) == Success(13))
  }

}

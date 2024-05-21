package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*
import effects.instance.IO

class TraversableTest extends AnyFunSuite {

  test("Basic traverse test") {
    val l:Seq[Int] = 1 to 10
    val res = l.traverse(Some(_).asInstanceOf[Option[Int]])
    assert(res.contains(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  }

  test("Basic traverse test as identity") {
    val l: Seq[Option[Int]] = (1 to 10).map(Some(_))
    val res = l.traverse(identity)
    assert(res.contains(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  }
}

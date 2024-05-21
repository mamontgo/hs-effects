package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*

class FoldableTest extends AnyFunSuite {

  test("foldMap") {
    val l:Seq[Int] = 1 to 3
    val f:Int => Seq[Int] = 1 to (_:Int)
    val m = l.foldMap(f)
    assert(m == Seq(1, 1, 2, 1, 2, 3))

    val temp: Range.Inclusive = 1 to 10
    val x = temp.take(10)

  }
}

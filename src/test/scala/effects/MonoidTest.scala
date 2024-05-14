package effects

import effects.All.*
import org.scalatest.funsuite.AnyFunSuite

class MonoidTest extends AnyFunSuite {

  test("test mconcat") {
    val x:Seq[Seq[Int]] = (1 to 10) <\> (Seq(_))

//    IO.runEffect(println(
//      Monoid.mconcat(x.map(_.monoid))
//    ))

    assert(Monoid.mconcat(x.map(_.monoid)) == (1 to 10))
  }
}

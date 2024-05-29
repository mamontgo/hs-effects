package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*
import effects.instance.IO
import effects.syntax.FunctionSyntax.FunctionApplication

class FunctorTest extends AnyFunSuite {

  test("liftF test") {

    val addOne:Int => Int = _+1
    val addOneF = liftF(addOne)

    IO.runEffect (println <-> addOneF (Some(43)))
    IO.runEffect (println <-> addOneF (Seq(43, 44)))

    assert(addOneF(Some(43)).contains(44))
    assert(addOneF (Seq(43, 44)) == Seq(44, 45))

  }

  test("infix examples") {
    val s = Seq(1, 2, 3, 4)
    val mapsAreSame = s.map(_+1) == s <\> (_+1)
    IO.runEffect(println ( s `map` (_+1)))
    assert(mapsAreSame)
  }

}

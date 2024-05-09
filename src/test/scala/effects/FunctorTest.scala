package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.instance.All.*
import effects.instance.IO
import effects.syntax.FunctionSyntax.FunctionApplication

class FunctorTest extends AnyFunSuite {

  test("liftF test") {

    val addOne:Int => Int = _+1
    val addOneF = Functor.liftF(addOne)

    IO.runEffect (println <> addOneF (Some(43)))
    IO.runEffect (println <> addOneF (Seq(43, 44)))

    assert(addOneF(Some(43)).contains(44))
    assert(addOneF (Seq(43, 44)) == Seq(44, 45))
    
  }

}

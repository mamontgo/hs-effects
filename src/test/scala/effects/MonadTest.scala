package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.instance.All.*
import effects.instance.IO

class MonadTest extends AnyFunSuite {

  test("monad sequence") {
    val x = Seq(IO.create("123"), IO.create("321"))
    Monad.sequence(x)
  }

}

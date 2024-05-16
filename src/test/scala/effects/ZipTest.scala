package effects

import org.scalatest.funsuite.AnyFunSuite
import effects.All.*
class ZipTest extends AnyFunSuite {

  test("zip seq") {
    val seq = (1 to 10).map(Some(_))
    val add = curry((_:Int)+(_:Int))
    val result = Zip.zipAll(add, Some(0).inst, seq)
    assert(result.contains(55))
  }
}

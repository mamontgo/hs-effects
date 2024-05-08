package effects.data

import org.scalatest.funsuite.AnyFunSuite
import effects.instance.IO

class IOTest extends AnyFunSuite {

  test("IO type is private") {
    val x = IO.create("Hello")
    // can not access the underlying inner type

  }
}

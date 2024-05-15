package effects.instance

import org.scalatest.funsuite.AnyFunSuite

class ThreadTest extends AnyFunSuite{

  test("Thread Should not run") {
    ThreadIO.sleep(100000)
  }
}

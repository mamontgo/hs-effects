package effects.instance

import effects.Functor.*
import org.scalatest.funsuite.AnyFunSuite
import effects.instance.OptionInstances.*

class OptionTest extends AnyFunSuite {

  test("fold implementation") {
//    val res = (_: Option[Int]).fold("hello")(x => x.toString)
    assert(None.foldLeft("Hello")((b, i) => i.toString) == "Hello")
    assert(Some(200).foldLeft("Hello")((b, i) => i.toString) == "200")
  }

  test("lift function map for option") {
    val x = Some(300)
    val inc = (_:Int)+1
    val incF = liftF(inc)

    assert(incF(x).contains(301))
  }

  test("monad on Option") {
    val res = for {
      x <- Some(1)
      y <- Some(100)
    } yield x + y


    assert(res.contains(101))
  }

}

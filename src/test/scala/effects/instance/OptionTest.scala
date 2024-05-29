package effects.instance

import effects.All.*
import effects.{Functor}
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class OptionTest extends AnyFunSuite {

  test("fold implementation") {
    assert(None.foldLeft("Hello")((b, i) => i.toString) == "Hello")
    assert(Some(200).foldLeft("Hello")((b, i) => i.toString) == "200")
  }

  test("lift function map for option") {
    val x = Some(300)
    val inc = (_:Int)+1
    val incF = Functor.liftF(inc)

    assert(incF(x).contains(301))
  }

  test("monad on Option") {
    val res = for {
      x <- Some(1)
      y <- Some(100)
    } yield x + y

    assert(res.contains(101))
  }

  test("monoid option test") {
    val f = Some(Seq(1,2))
    val s = Some(Seq(3,4))

    IO.runEffect(println(f.combine(s)))
    assert(f.combine(s) == Option(Seq(1,2,3,4)))
  }

  test("option zipWith") {
    val res = Some(10).zipWith(Some(20))(curry((_:Int)+(_:Int)))
    assert(res.contains(30))
  }

  test("traverse seq of options") {
    val x:Seq[Option[Int]] = Seq(Some(1), Some(2))
    assert(x.traverse(identity).contains(Seq(1,2)))
    val y:Seq[Option[Int]] = Seq(Some(1), Some(2), None)
    assert(y.traverse(identity).isEmpty)

  }

  test("alternative options") {
    assert(Some(1).alt(None).contains(1))
    assert((Some(1) <|> None).contains(1))

    assert(None.alt(Some(1)).contains(1))
    assert((None <|> Some(1)).contains(1))
    assert((None <|> None).isEmpty)

    assert(Some(1).alt(Some(2)).contains(1))

  }
}

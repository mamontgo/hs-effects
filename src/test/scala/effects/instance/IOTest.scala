package effects.instance

import effects.Monad
import effects.All.*
import org.scalatest.funsuite.AnyFunSuite

class IOTest extends AnyFunSuite {


  test("IO combine") {

    val x:IO[Seq[Int]] = IO.create(Seq(1,2,3))
    val y:IO[Seq[Int]] = IO.create(Seq(4,5,6))
    val m:IO[Seq[Int]] = x.map(_.monoid).combine(y)

    val res:Seq[Int] = IO.runEffect(m)
    assert(res == Seq(1, 2, 3, 4, 5, 6))


  }

  test("sequence IO ") {
    val s: Seq[Int] = 1 to 10
    val r:Seq[IO[Unit]] = s.map(v => println(s"This is $v"))

    IO.runEffect(println(r))

    val y:IO[Seq[Unit]] = Monad.sequence(r.map(_.monad))
    IO.runEffect(y)

  }
}

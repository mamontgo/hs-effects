package effects.instance

import effects.All.*
import effects.Monad
import org.scalatest.funsuite.AnyFunSuite

class IOTest extends AnyFunSuite {


  test("IO combine") {

    val x:IO[Seq[Int]] = IO.create(Seq(1,2,3))
    val y:IO[Seq[Int]] = IO.create(Seq(4,5,6))
    val m:IO[Seq[Int]] = x.combine(y)

    val res:Seq[Int] = IO.runEffect(m)
    assert(res == Seq(1, 2, 3, 4, 5, 6))

    val s:String = "Hello!"


  }

  test("sequence IO ") {
    val s: Seq[Int] = 1 to 10
    val r:Seq[IO[Unit]] = s.map(v => println(s"This is $v"))

    IO.runEffect(println(r))

    val y:IO[Seq[Unit]] = Monad.sequence(r)
    IO.runEffect(y)
  }

  test("sequence IO as traversable") {
    val s: Seq[Int] = 1 to 10
    val r: Seq[IO[Unit]] = s.map(v => println(s"This is $v"))
    IO.runEffect(sequenceA(r))
  }

  test("sequence Option IO as traversable") {
    val intOption: Option[Int] = Some(10)
    val printlnOption: Option[IO[Unit]] = intOption.map(v => println(s"This is $v"))
    val x: IO[Option[Unit]] = sequenceA(printlnOption)
    IO.runEffect(x)
//    printlnOption.map(IO.runEffect)
  }

  def maybeAddOne(x: Int): Int = if (x == 0) throw Exception("Yeah") else x + 1


  test("try run IO with exception") {

//    IO.runEffect(IO.create(maybeAddOne(0)))

    IO.tryRun(IO.create(maybeAddOne(10))) match
      case Right(x) => assert(x == 11)
      case _ => fail("Should be right")

    IO.tryRun(IO.create(maybeAddOne(0))) match
      case Left(x) => assert(x.getMessage == "Yeah")
      case _ => fail("Should be error")

  }

  test("alternative result") {
    val alt1: IO[Int] = IO.create(maybeAddOne(0)) <|> IO.create(maybeAddOne(10))
    val alt2: IO[Int] = IO.create(maybeAddOne(10)) <|> IO.create(maybeAddOne(1))
    assert(IO.runEffect(alt1) == 11)
    assert(IO.runEffect(alt2) == 11)
  }
}
